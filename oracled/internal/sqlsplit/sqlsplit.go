// Package sqlsplit は SQL のテキストを文に切り分ける。
//
// daemon がこれを持つ理由（設計 §7）。
//
//   - OCI / ドライバは末尾の `;` を受け付けない（ORA-00911 / ORA-00933）。
//     一方 PL/SQL ブロックは中の `;` が必須。つまり誰かが必ず判定する
//   - `@foo.sql` は `;` ではなく行末で終わる。だから daemon は
//     「SQL*Plus のコマンドを実行しない」が「それが SQL でないことを
//     見分ける責任は持つ」
//   - 同じ規則を Emacs 側にも書くと lexer が 2 つになり、必ず食い違う
//
// 【重要】位置は**文字（rune）単位**で返す。Emacs は文字で数えるので、
// バイト単位で返すと日本語を含む文でずれる。
package sqlsplit

import (
	"strings"
	"unicode"
)

type Kind string

const (
	KindSQL    Kind = "sql"    // SELECT / DML / DDL。終端子は `;` か単独行の `/`
	KindPLSQL  Kind = "plsql"  // PL/SQL ブロック。終端子は単独行の `/` だけ
	KindClient Kind = "client" // SQL*Plus のコマンド。daemon は実行しない
)

// Statement は 1 つの文。
type Statement struct {
	// Text は Oracle に投げる本体（終端子を外し、前後の空白を落としたもの）。
	// Kind が client のときはコマンド行そのもの。
	Text string `json:"text"`
	// Raw は元のテキストの該当範囲（終端子を含む）。echo に使う。
	Raw string `json:"raw"`
	// Command は Kind が client のときの先頭語（大文字）。`@` と `@@` はそのまま。
	Command string `json:"command,omitempty"`
	Kind    Kind   `json:"kind"`
	// Begin / End は文字単位の位置（End は終端子の次）。
	Begin int `json:"begin"`
	End   int `json:"end"`
	// Line は Begin のある行（1 始まり）。
	Line int `json:"line"`
	// Complete は文として閉じているか。対話バッファの RET の判定に使う。
	// 閉じていない例: 終端子が無い / 文字列リテラルの途中 / `END;` の後に
	// `/` が来ていない PL/SQL ブロック。
	Complete bool `json:"complete"`
	// Binds は文の中のバインド変数（`:name`）。Emacs はこれを見て値を訊く。
	Binds []string `json:"binds,omitempty"`
}

// clientCommands は「SQL ではない」と見分ける先頭語。
// 実行はしない（Emacs 側のクライアントコマンドになる。設計 §12）。
var clientCommands = map[string]bool{
	"@": true, "@@": true, "!": true, "$": true,
	"ACCEPT": true, "ARCHIVE": true, "BREAK": true, "BTITLE": true,
	"CLEAR": true, "COL": true, "COLUMN": true, "COMPUTE": true,
	"CONNECT": true, "COPY": true, "DEF": true, "DEFINE": true,
	"DESC": true, "DESCRIBE": true, "DISCONNECT": true, "EDIT": true,
	"EXEC": true, "EXECUTE": true, "EXIT": true, "GET": true,
	"HOST": true, "INFO": true, "L": true, "LIST": true, "PAUSE": true,
	"PRINT": true, "PROMPT": true, "QUIT": true, "R": true, "REM": true,
	"REMARK": true, "RUN": true, "SAVE": true, "SET": true, "SHOW": true,
	"SPOOL": true, "START": true, "STORE": true, "TIMING": true,
	"TTITLE": true, "UNDEF": true, "UNDEFINE": true, "VAR": true,
	"VARIABLE": true, "WHENEVER": true,
}

// setIsSQL は `SET` で始まるが SQL である組み合わせ。
// SET TRANSACTION / SET ROLE / SET CONSTRAINT(S) は本物の SQL なので、
// SQL*Plus の SET と混ぜてはいけない。
var setIsSQL = map[string]bool{
	"TRANSACTION": true, "ROLE": true, "CONSTRAINT": true, "CONSTRAINTS": true,
}

// plsqlObjects は CREATE [OR REPLACE] の後ろがこれなら PL/SQL
// （中に `;` を含むので `/` で終わる）。
var plsqlObjects = map[string]bool{
	"PROCEDURE": true, "FUNCTION": true, "PACKAGE": true, "TRIGGER": true,
	"TYPE": true, "LIBRARY": true, "JAVA": true, "ASSEMBLY": true,
}

type scanner struct {
	r    []rune
	i    int
	line int
}

// Split はテキストを文の配列に切り分ける。
func Split(src string) []Statement {
	s := &scanner{r: []rune(src), i: 0, line: 1}
	var out []Statement
	for {
		s.skipGap()
		if s.eof() {
			return out
		}
		if st, ok := s.next(); ok {
			out = append(out, st)
		}
	}
}

func (s *scanner) eof() bool { return s.i >= len(s.r) }

func (s *scanner) at(off int) rune {
	if s.i+off < 0 || s.i+off >= len(s.r) {
		return 0
	}
	return s.r[s.i+off]
}

func (s *scanner) advance() rune {
	c := s.r[s.i]
	s.i++
	if c == '\n' {
		s.line++
	}
	return c
}

// skipGap は文の手前の空白とコメントを飛ばす。
// 文と文の間のコメントはどの文にも属さない（投げるものが無い）。
func (s *scanner) skipGap() {
	for !s.eof() {
		c := s.at(0)
		switch {
		case unicode.IsSpace(c):
			s.advance()
		case c == '-' && s.at(1) == '-':
			s.skipLineComment()
		case c == '/' && s.at(1) == '*':
			s.skipBlockComment()
		default:
			return
		}
	}
}

func (s *scanner) skipLineComment() {
	for !s.eof() && s.at(0) != '\n' {
		s.advance()
	}
}

func (s *scanner) skipBlockComment() {
	s.advance() // /
	s.advance() // *
	for !s.eof() {
		if s.at(0) == '*' && s.at(1) == '/' {
			s.advance()
			s.advance()
			return
		}
		s.advance()
	}
}

// skipString は `'...'`（`”` でエスケープ）を飛ばす。閉じていなければ false。
func (s *scanner) skipString() bool {
	s.advance() // 開きの '
	for !s.eof() {
		if s.at(0) == '\'' {
			if s.at(1) == '\'' { // '' は 1 文字の '
				s.advance()
				s.advance()
				continue
			}
			s.advance()
			return true
		}
		s.advance()
	}
	return false
}

// closingDelim は q'[ ... ]' の閉じ文字を返す。
func closingDelim(open rune) rune {
	switch open {
	case '[':
		return ']'
	case '(':
		return ')'
	case '{':
		return '}'
	case '<':
		return '>'
	default:
		return open
	}
}

// skipQuoted は q'X...X' / nq'X...X' を飛ばす。呼ぶ前に isQuoteStart で見る。
func (s *scanner) skipQuoted() bool {
	if s.at(0) == 'n' || s.at(0) == 'N' {
		s.advance()
	}
	s.advance() // q
	s.advance() // '
	if s.eof() {
		return false
	}
	close := closingDelim(s.advance())
	for !s.eof() {
		if s.at(0) == close && s.at(1) == '\'' {
			s.advance()
			s.advance()
			return true
		}
		s.advance()
	}
	return false
}

// isQuoteStart は現在位置が q'…' / nq'…' の始まりかを見る。
func (s *scanner) isQuoteStart() bool {
	off := 0
	if s.at(0) == 'n' || s.at(0) == 'N' {
		off = 1
	}
	c := s.at(off)
	return (c == 'q' || c == 'Q') && s.at(off+1) == '\''
}

// skipIdent は `"..."` の引用識別子を飛ばす。
func (s *scanner) skipIdent() bool {
	s.advance()
	for !s.eof() {
		if s.advance() == '"' {
			return true
		}
	}
	return false
}

// atSlashLine は現在位置が「単独行の `/`」の先頭かを見る。
// PL/SQL の終端子で、SQL でも SQL*Plus 流儀の実行指示になる。
// 手前が行頭（空白のみ）で、`/` の後ろが行末まで空白であることが条件。
func (s *scanner) atSlashLine() bool {
	if s.at(0) != '/' || s.at(1) == '*' {
		return false
	}
	for j := s.i - 1; j >= 0; j-- {
		c := s.r[j]
		if c == '\n' {
			break
		}
		if !unicode.IsSpace(c) {
			return false
		}
	}
	for j := s.i + 1; j < len(s.r); j++ {
		c := s.r[j]
		if c == '\n' {
			break
		}
		if !unicode.IsSpace(c) {
			return false
		}
	}
	return true
}

// peekWord は現在位置から数えて n 番目の語を大文字で返す（コメントは飛ばす）。
func (s *scanner) peekWords(n int) []string {
	save, saveLine := s.i, s.line
	defer func() { s.i, s.line = save, saveLine }()
	var words []string
	for len(words) < n {
		s.skipGap()
		if s.eof() {
			break
		}
		c := s.at(0)
		if !unicode.IsLetter(c) && c != '_' && c != '@' && c != '!' && c != '$' {
			break
		}
		start := s.i
		if c == '@' {
			s.advance()
			if s.at(0) == '@' {
				s.advance()
			}
		} else if c == '!' || c == '$' {
			s.advance()
		} else {
			for !s.eof() {
				d := s.at(0)
				if unicode.IsLetter(d) || unicode.IsDigit(d) || d == '_' || d == '#' || d == '$' {
					s.advance()
					continue
				}
				break
			}
		}
		words = append(words, strings.ToUpper(string(s.r[start:s.i])))
	}
	return words
}

// classify は文の種類を決める。位置は動かさない。
func (s *scanner) classify() (Kind, string) {
	w := s.peekWords(4)
	if len(w) == 0 {
		return KindSQL, ""
	}
	head := w[0]
	if head == "SET" && len(w) > 1 && setIsSQL[w[1]] {
		return KindSQL, "" // SET TRANSACTION / ROLE / CONSTRAINTS は本物の SQL
	}
	if clientCommands[head] {
		return KindClient, head
	}
	switch head {
	case "DECLARE", "BEGIN":
		return KindPLSQL, ""
	case "CREATE":
		rest := w[1:]
		// OR REPLACE / EDITIONABLE / NONEDITIONABLE を飛ばす
		for len(rest) > 0 {
			switch rest[0] {
			case "OR", "REPLACE", "EDITIONABLE", "NONEDITIONABLE":
				rest = rest[1:]
				continue
			}
			break
		}
		if len(rest) > 0 && plsqlObjects[rest[0]] {
			return KindPLSQL, ""
		}
		// PACKAGE BODY / TYPE BODY も上で当たる（先頭語が PACKAGE / TYPE）
		return KindSQL, ""
	}
	return KindSQL, ""
}

// next は 1 文を切り出す。切り出せなかったとき（単独の `/` など）は false。
func (s *scanner) next() (Statement, bool) {
	begin, beginLine := s.i, s.line

	// 単独行の `/` は「直前の文をもう一度実行する」ために使われるが、
	// この設計では何もしない（SQLcl の挙動を再現しない。設計 §7）。
	if s.atSlashLine() {
		s.advance()
		return Statement{}, false
	}
	// 裸の `;` も何もしない。
	if s.at(0) == ';' {
		s.advance()
		return Statement{}, false
	}

	kind, command := s.classify()

	if kind == KindClient {
		for !s.eof() && s.at(0) != '\n' {
			s.advance()
		}
		raw := string(s.r[begin:s.i])
		text := strings.TrimRight(strings.TrimSpace(raw), ";")
		return Statement{
			Text: strings.TrimSpace(text), Raw: raw, Command: command,
			Kind: kind, Begin: begin, End: s.i, Line: beginLine, Complete: true,
		}, true
	}

	complete := false
	bodyEnd := -1 // 終端子の手前（Text の終わり）
	for !s.eof() {
		c := s.at(0)
		switch {
		case c == '-' && s.at(1) == '-':
			s.skipLineComment()
			continue
		case c == '/' && s.at(1) == '*':
			s.skipBlockComment()
			continue
		case c == '\'':
			if !s.skipString() {
				// 閉じていない文字列。未完として返す
				bodyEnd = s.i
				goto done
			}
			continue
		case c == '"':
			if !s.skipIdent() {
				bodyEnd = s.i
				goto done
			}
			continue
		case (c == 'q' || c == 'Q' || c == 'n' || c == 'N') && s.isQuoteStart():
			if !s.skipQuoted() {
				bodyEnd = s.i
				goto done
			}
			continue
		case c == ';' && kind == KindSQL:
			bodyEnd = s.i
			s.advance()
			complete = true
			goto done
		case s.atSlashLine():
			// PL/SQL の終端子。SQL でも「ここまでを実行」の意味で受ける
			bodyEnd = s.i
			s.advance()
			complete = true
			goto done
		}
		s.advance()
	}
	bodyEnd = s.i

done:
	if bodyEnd < 0 {
		bodyEnd = s.i
	}
	raw := string(s.r[begin:s.i])
	text := strings.TrimSpace(string(s.r[begin:bodyEnd]))
	if text == "" {
		return Statement{}, false
	}
	return Statement{
		Text: text, Raw: raw, Kind: kind, Binds: Binds(text),
		Begin: begin, End: s.i, Line: beginLine, Complete: complete,
	}, true
}
