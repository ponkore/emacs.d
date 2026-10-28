package sqlsplit

// 先頭の語で文の性質を見分けるための小道具。
// daemon が「問い合わせか / 暗黙コミットを起こすか」を決めるのに使う。

// FirstWords は先頭から n 語を大文字で返す（コメントは飛ばす）。
func FirstWords(src string, n int) []string {
	s := &scanner{r: []rune(src), i: 0, line: 1}
	return s.peekWords(n)
}

// IsQuery は結果セットを返す文か（QueryContext に回すか）。
func IsQuery(src string) bool {
	w := FirstWords(src, 1)
	if len(w) == 0 {
		return false
	}
	switch w[0] {
	case "SELECT", "WITH":
		return true
	}
	return false
}

// ddlStarters は Oracle が暗黙コミットする文の先頭語。
// 【重要】これを取りこぼすと、autocommit OFF のつもりのユーザーに
// 「確定してしまったこと」を知らせられない（実測: tx 中の DDL は
// 保留中の DML を巻き込んでコミットする。phase0-findings.md I）。
var ddlStarters = map[string]bool{
	"CREATE": true, "ALTER": true, "DROP": true, "TRUNCATE": true,
	"RENAME": true, "GRANT": true, "REVOKE": true, "COMMENT": true,
	"ANALYZE": true, "AUDIT": true, "NOAUDIT": true, "FLASHBACK": true,
	"PURGE": true, "ASSOCIATE": true, "DISASSOCIATE": true,
}

// IsDDL は暗黙コミットを起こす文か。
func IsDDL(src string) bool {
	w := FirstWords(src, 1)
	if len(w) == 0 {
		return false
	}
	return ddlStarters[w[0]]
}

// Classify は文を "query" / "ddl" / "plsql" / "tx" / "dml" に分ける。
// Emacs 側の文言（「3 行が更新されました」「PL/SQL が完了しました」）を
// 決めるのに使う。**Emacs 側に先頭語の判定を書かせない**ためのもの。
func Classify(src string) string {
	w := FirstWords(src, 2)
	if len(w) == 0 {
		return "dml"
	}
	if IsQuery(src) {
		return "query"
	}
	if _, ok := IsTxControl(src); ok {
		return "tx"
	}
	switch w[0] {
	case "DECLARE", "BEGIN":
		return "plsql"
	}
	if IsDDL(src) {
		// CREATE PROCEDURE などは PL/SQL だが、暗黙コミットもする。
		// 表示の都合では DDL 扱いでよい
		return "ddl"
	}
	return "dml"
}

// IsTxControl は COMMIT / ROLLBACK / SAVEPOINT か。
// daemon 側の tx 状態と食い違わないよう、専用の経路に回す。
func IsTxControl(src string) (string, bool) {
	w := FirstWords(src, 1)
	if len(w) == 0 {
		return "", false
	}
	switch w[0] {
	case "COMMIT", "ROLLBACK", "SAVEPOINT":
		return w[0], true
	}
	return "", false
}

// Binds は文の中のバインド変数（`:name`）を現れた順に返す。
//
// 【重要】ここも daemon が持つ。Emacs 側で正規表現を書くと、文字列の
// 中の `:` やコメントの中の `:name`、`::` を取り違える（lexer を 2 つに
// しない原則。設計 §7）。
//
// PL/SQL のラベル（`<<label>>`）や代入（`:=`）は拾わない。
func Binds(src string) []string {
	s := &scanner{r: []rune(src), i: 0, line: 1}
	var out []string
	seen := map[string]bool{}
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
				return out
			}
			continue
		case c == '"':
			if !s.skipIdent() {
				return out
			}
			continue
		case (c == 'q' || c == 'Q' || c == 'n' || c == 'N') && s.isQuoteStart():
			if !s.skipQuoted() {
				return out
			}
			continue
		case c == ':':
			// `::` は型変換などで使われることがあるので飛ばす。
			// `:=` は代入。どちらもバインドではない
			if s.at(1) == ':' || s.at(1) == '=' {
				s.advance()
				s.advance()
				continue
			}
			s.advance()
			start := s.i
			for !s.eof() {
				d := s.at(0)
				if isIdentRune(d) {
					s.advance()
					continue
				}
				break
			}
			name := string(s.r[start:s.i])
			// 位置指定のバインド（`:1`）は名前として扱わない
			if name != "" && !allDigits(name) && !seen[name] {
				seen[name] = true
				out = append(out, name)
			}
			continue
		}
		s.advance()
	}
	return out
}

func isIdentRune(c rune) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
		(c >= '0' && c <= '9') || c == '_' || c == '#' || c == '$'
}

func allDigits(s string) bool {
	for _, c := range s {
		if c < '0' || c > '9' {
			return false
		}
	}
	return len(s) > 0
}
