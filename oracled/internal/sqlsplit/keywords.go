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
