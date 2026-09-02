//! magit-gitd — Emacs の magit から git の実行を肩代わりする常駐プロセス。
//!
//! 段階 2a: 素通しプロキシ。キャッシュも先読みもファイル監視もしない。
//! Emacs の `call-process` が Windows で遅い (同じ cmd.exe を起動するのに
//! PowerShell 20ms / Emacs 60-76ms) ぶんを迂回することだけが目的。
//!
//! プロトコル: stdio 上の JSON-RPC 2.0 / LSP 方式のフレーミング
//! (`Content-Length: N\r\n\r\n<json>`)。Emacs 側は同梱の jsonrpc.el を使う。
//!
//! 設計上の約束:
//!   - git の「意味」を持たない。渡されたものをそのまま実行して返すだけ。
//!   - 環境変数は env_clear() してから Emacs に渡されたものだけを使う。
//!     継承した環境が混ざると「デーモン経由のときだけ挙動が違う」バグになる。
//!   - stdout は生バイトを base64 で返す。git の出力は任意のバイト列を
//!     含みうるので JSON 文字列には直接入らない。
//!   - stdin が EOF になったら終了する。Emacs が死んだときの後始末はこれで足りる。
//!   - 2a は単一スレッド。Emacs は応答を待ってブロックしているので並行性に
//!     価値がない。並列化は 2b の先読みで初めて要る。

use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Write};
use std::process::Command;
use std::time::Instant;

use serde::Deserialize;
use serde_json::{json, Value};

/// Emacs 側と揃える。不一致なら Emacs は機能を無効化する。
const PROTOCOL: u32 = 1;

/// JSON-RPC のエラーコード。-32000 未満は実装定義の領域。
const E_METHOD_NOT_FOUND: i32 = -32601;
const E_INVALID_PARAMS: i32 = -32602;
/// 未登録の env id。git は起動していないので Emacs 側は再実行して安全。
const E_UNKNOWN_ENV: i32 = -32001;
/// プロセスの起動自体に失敗した。同じく git は起動していない。
const E_SPAWN_FAILED: i32 = -32002;

// ---------------------------------------------------------------- フレーミング

/// `Content-Length: N\r\n\r\n` + 本体 N バイトを 1 つ読む。
/// EOF なら `Ok(None)` を返す (= 終了の合図)。
fn read_frame<R: BufRead>(r: &mut R) -> io::Result<Option<Vec<u8>>> {
    let mut len: Option<usize> = None;
    loop {
        let mut line = String::new();
        if r.read_line(&mut line)? == 0 {
            return Ok(None); // EOF
        }
        let t = line.trim_end_matches(['\r', '\n']);
        if t.is_empty() {
            break; // ヘッダの終わり
        }
        if let Some(v) = t.strip_prefix("Content-Length:") {
            len = v.trim().parse::<usize>().ok();
        }
        // 他のヘッダ (Content-Type など) は読み飛ばす
    }
    let Some(len) = len else {
        // Content-Length の無いフレームは解釈できない。無視して次へ。
        return Ok(Some(Vec::new()));
    };
    let mut buf = vec![0u8; len];
    r.read_exact(&mut buf)?;
    Ok(Some(buf))
}

fn write_frame<W: Write>(w: &mut W, body: &str) -> io::Result<()> {
    // Content-Length はバイト数。str::len() はバイト長なのでそのまま使える。
    write!(w, "Content-Length: {}\r\n\r\n", body.len())?;
    w.write_all(body.as_bytes())?;
    w.flush()
}

// ---------------------------------------------------------------- base64

/// base64 (標準アルファベット、パディングあり)。
/// エンコードしかしないので依存クレートは足さない。
fn b64(data: &[u8]) -> String {
    const T: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity(data.len().div_ceil(3) * 4);
    for c in data.chunks(3) {
        let b0 = c[0] as u32;
        let b1 = *c.get(1).unwrap_or(&0) as u32;
        let b2 = *c.get(2).unwrap_or(&0) as u32;
        let n = (b0 << 16) | (b1 << 8) | b2;
        out.push(T[(n >> 18 & 63) as usize] as char);
        out.push(T[(n >> 12 & 63) as usize] as char);
        out.push(if c.len() > 1 { T[(n >> 6 & 63) as usize] as char } else { '=' });
        out.push(if c.len() > 2 { T[(n & 63) as usize] as char } else { '=' });
    }
    out
}

// ---------------------------------------------------------------- リクエスト

#[derive(Deserialize)]
struct EnvRegister {
    id: String,
    /// "NAME=VALUE" の並び。Emacs の process-environment そのまま。
    env: Vec<String>,
}

#[derive(Deserialize)]
struct GitRun {
    /// Emacs が解決した git の実体。デーモン側では探さない
    /// (magit-git-executable の cygpath 経由の解決を二重に持たないため)。
    program: String,
    /// toplevel ではなく default-directory をそのまま受け取る。
    cwd: String,
    args: Vec<String>,
    /// env/register で登録した id。
    env: String,
    /// BUFFER が (t "FILE") のときだけ true。
    #[serde(default)]
    want_stderr: bool,
}

// ---------------------------------------------------------------- 本体

fn main() {
    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin.lock());
    let stdout = io::stdout();
    let mut writer = stdout.lock();

    // env id -> [(NAME, VALUE)]
    let mut envs: HashMap<String, Vec<(String, String)>> = HashMap::new();

    loop {
        let body = match read_frame(&mut reader) {
            Ok(Some(b)) => b,
            Ok(None) => break, // stdin EOF = Emacs が終了した
            Err(e) => {
                eprintln!("フレーム読み取りに失敗: {e}");
                break;
            }
        };
        if body.is_empty() {
            continue;
        }

        let msg: Value = match serde_json::from_slice(&body) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("JSON の解析に失敗: {e}");
                continue;
            }
        };

        let id = msg.get("id").cloned();
        let method = msg.get("method").and_then(Value::as_str).unwrap_or("");
        let params = msg.get("params").cloned().unwrap_or(Value::Null);

        let outcome: Result<Value, (i32, String)> = match method {
            "initialize" => Ok(json!({
                "protocol": PROTOCOL,
                "version": concat!("magit-gitd ", env!("CARGO_PKG_VERSION")),
                "pid": std::process::id(),
            })),

            "env/register" => match serde_json::from_value::<EnvRegister>(params) {
                Ok(p) => {
                    let parsed = p
                        .env
                        .iter()
                        .filter_map(|kv| {
                            // 値に "=" を含む変数があるので split_once を使う。
                            // Windows の "=C:=C:\..." のような特殊な項目は捨てる。
                            kv.split_once('=')
                                .filter(|(k, _)| !k.is_empty())
                                .map(|(k, v)| (k.to_string(), v.to_string()))
                        })
                        .collect::<Vec<_>>();
                    envs.insert(p.id, parsed);
                    Ok(json!({ "ok": true }))
                }
                Err(e) => Err((E_INVALID_PARAMS, e.to_string())),
            },

            "git/run" => match serde_json::from_value::<GitRun>(params) {
                Ok(p) => run_git(&p, &envs),
                Err(e) => Err((E_INVALID_PARAMS, e.to_string())),
            },

            "shutdown" => {
                respond(&mut writer, &id, Ok(json!({ "ok": true })));
                break;
            }

            other => Err((E_METHOD_NOT_FOUND, format!("未知のメソッド: {other}"))),
        };

        // id が無いものは通知。応答してはいけない。
        if id.is_some() {
            respond(&mut writer, &id, outcome);
        }
    }
}

fn run_git(p: &GitRun, envs: &HashMap<String, Vec<(String, String)>>) -> Result<Value, (i32, String)> {
    let Some(env) = envs.get(&p.env) else {
        // git は起動していない。Emacs 側は登録し直して再送してよい。
        return Err((E_UNKNOWN_ENV, format!("未登録の env id: {}", p.env)));
    };

    let t0 = Instant::now();
    let out = Command::new(&p.program)
        .current_dir(&p.cwd)
        .args(&p.args)
        .env_clear()
        .envs(env.iter().cloned())
        .output();

    match out {
        Ok(o) => Ok(json!({
            // Windows では常に Some。念のため。
            "exit": o.status.code().unwrap_or(-1),
            "stdout": b64(&o.stdout),
            "stderr": if p.want_stderr { Value::String(b64(&o.stderr)) } else { Value::Null },
            "elapsed_ms": t0.elapsed().as_millis() as u64,
        })),
        // spawn 自体に失敗した = git は 1 度も動いていない。
        // Emacs 側はこれを見て素通しで再実行してよい (書き込みでも安全)。
        Err(e) => Err((E_SPAWN_FAILED, format!("{} の起動に失敗: {e}", p.program))),
    }
}

fn respond<W: Write>(w: &mut W, id: &Option<Value>, outcome: Result<Value, (i32, String)>) {
    let msg = match outcome {
        Ok(result) => json!({
            "jsonrpc": "2.0",
            "id": id.clone().unwrap_or(Value::Null),
            "result": result,
        }),
        Err((code, message)) => json!({
            "jsonrpc": "2.0",
            "id": id.clone().unwrap_or(Value::Null),
            "error": { "code": code, "message": message },
        }),
    };
    let body = serde_json::to_string(&msg).unwrap_or_else(|e| {
        format!(r#"{{"jsonrpc":"2.0","id":null,"error":{{"code":-32603,"message":"{e}"}}}}"#)
    });
    if let Err(e) = write_frame(w, &body) {
        eprintln!("応答の書き込みに失敗: {e}");
    }
}
