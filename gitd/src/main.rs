//! magit-gitd — Emacs の magit から git の実行を肩代わりする常駐プロセス。
//!
//! 段階 2a: 素通しプロキシ。Emacs の `call-process` が Windows で遅い
//! (同じ cmd.exe を起動するのに PowerShell 20ms / Emacs 60-76ms) ぶんを迂回した。
//! 段階 2b: キャッシュ + 並列先読み。残っていた「29 回の git 起動を直列に
//! 待つ 0.6 秒」を潰す。
//!
//! プロトコル: stdio 上の JSON-RPC 2.0 / LSP 方式のフレーミング
//! (`Content-Length: N\r\n\r\n<json>`)。Emacs 側は同梱の jsonrpc.el を使う。
//!
//! 設計上の約束:
//!   - git の「意味」を持たない。渡されたものをそのまま実行して返すだけ。
//!     **キャッシュしてよいかどうかも Emacs が `role` で指示する。**
//!     デーモンが git のサブコマンドを解釈し始めると、magit 側の事情
//!     (どの引数が書き込みか) が 2 か所に散らばって必ず食い違う。
//!   - 環境変数は env_clear() してから Emacs に渡されたものだけを使う。
//!     継承した環境が混ざると「デーモン経由のときだけ挙動が違う」バグになる。
//!   - stdout は生バイトを base64 で返す。git の出力は任意のバイト列を
//!     含みうるので JSON 文字列には直接入らない。
//!   - stdin が EOF になったら終了する。Emacs が死んだときの後始末はこれで足りる。
//!
//! ## キャッシュの正しさ (2b でいちばん難しいところ)
//!
//! 古い答えを返すキャッシュは**静かに壊れる**。magit が事実と違う内容を表示し、
//! しかもユーザはそれに気づけない。そこで無効化を「通知」ではなく
//! **トークン**で表現している。
//!
//! Emacs は `git/run` のたびに `repo` と `token` を載せてくる。token は
//! Emacs 側が持っているリポジトリ状態の通し番号で、
//!
//!   - 監視 (my-magit-watch) が拾ったイベント 1 件ごと
//!   - magit-pre-refresh-hook (= magit 自身の書き込みと、ユーザの `g`)
//!
//! で増える。デーモンは (repo, token, コマンド) でキャッシュし、
//! token が違えば問答無用でミスにする。**無効化通知は存在しない**ので、
//! 「通知を 1 つ落とすと永久に古いまま」という壊れ方をしない。
//!
//! 詳細は ../tmp/magit-gitd-2b-design.md を参照。

use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Write};
use std::process::Command;
use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use std::time::Instant;

use serde::Deserialize;
use serde_json::{json, Value};

/// Emacs 側と揃える。不一致なら Emacs は機能を無効化する。
const PROTOCOL: u32 = 2;

/// JSON-RPC のエラーコード。-32000 未満は実装定義の領域。
const E_METHOD_NOT_FOUND: i32 = -32601;
const E_INVALID_PARAMS: i32 = -32602;
/// 未登録の env id。git は起動していないので Emacs 側は再実行して安全。
const E_UNKNOWN_ENV: i32 = -32001;
/// プロセスの起動自体に失敗した。同じく git は起動していない。
const E_SPAWN_FAILED: i32 = -32002;

/// 1 リポジトリあたりのキャッシュ上限。超えたら丸ごと捨てる
/// (LRU を持つほどの規模ではない。先読みですぐ埋め直される)。
const CACHE_CAP: usize = 256;
/// 先読みレシピの上限。magit のリフレッシュ 1 回は 29 コマンド程度。
const RECIPE_CAP: usize = 128;
/// prelude の上限。実際には update-index --refresh の 1 つだけ。
const PRELUDE_CAP: usize = 4;
/// 先読みを繰り返す上限。ビルド中に延々と回らないように。
const PREWARM_ROUNDS: usize = 3;
/// 並列度の上限。
const MAX_THREADS: usize = 8;

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
    /// 監視中のリポジトリのルート。無ければキャッシュも先読みもしない。
    #[serde(default)]
    repo: Option<String>,
    /// リポジトリ状態の通し番号。
    #[serde(default)]
    token: Option<u64>,
    /// "cache" / "prelude" / なし。
    #[serde(default)]
    role: Option<String>,
}

#[derive(Deserialize)]
struct RepoRef {
    repo: String,
    #[serde(default)]
    token: Option<u64>,
}

// ---------------------------------------------------------------- 実行の単位

/// 1 回の git 実行を一意に決めるもの。先読みで再生するのでそのまま保持する。
#[derive(Clone)]
struct Spec {
    program: String,
    cwd: String,
    args: Vec<String>,
    env: String,
}

impl Spec {
    /// キャッシュのキー。区切りは引数に現れない制御文字を使う。
    fn key(&self) -> String {
        let mut k = String::with_capacity(64);
        k.push_str(&self.env);
        k.push('\x1f');
        k.push_str(&self.cwd);
        k.push('\x1f');
        k.push_str(&self.program);
        for a in &self.args {
            k.push('\x1f');
            k.push_str(a);
        }
        k
    }
}

#[derive(Clone)]
struct RunResult {
    exit: i32,
    stdout: Arc<Vec<u8>>,
    stderr: Arc<Vec<u8>>,
    elapsed_ms: u64,
}

type Outcome = Result<RunResult, (i32, String)>;

/// 実行中の 1 件。同じコマンドを同時に 2 回起動しないための待ち合わせ点。
struct Job {
    token: u64,
    slot: Mutex<Option<Outcome>>,
    cv: Condvar,
}

impl Job {
    fn new(token: u64) -> Self {
        Job { token, slot: Mutex::new(None), cv: Condvar::new() }
    }
    fn complete(&self, r: Outcome) {
        *self.slot.lock().unwrap() = Some(r);
        self.cv.notify_all();
    }
    fn wait(&self) -> Outcome {
        let mut g = self.slot.lock().unwrap();
        while g.is_none() {
            g = self.cv.wait(g).unwrap();
        }
        g.clone().unwrap()
    }
}

#[derive(Default)]
struct RepoState {
    /// いまキャッシュが表している世代。
    token: u64,
    cache: HashMap<String, RunResult>,
    inflight: HashMap<String, Arc<Job>>,
    /// 前回の先読み以降に Emacs が実際に要求した読み取りコマンド。
    observed: Vec<Spec>,
    /// 次の先読みで走らせるもの (= 直前のリフレッシュで使われたもの)。
    recipe: Vec<Spec>,
    /// 先読みの先頭で直列に走らせるもの。
    prelude: Vec<Spec>,
    prewarming: bool,
    dirty: bool,
    hits: u64,
    misses: u64,
    prewarms: u64,
}

struct Shared {
    envs: Mutex<HashMap<String, Arc<Vec<(String, String)>>>>,
    repos: Mutex<HashMap<String, RepoState>>,
    out: Mutex<io::Stdout>,
    threads: usize,
}

// ---------------------------------------------------------------- 本体

fn main() {
    let threads = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .clamp(1, MAX_THREADS);

    let sh = Arc::new(Shared {
        envs: Mutex::new(HashMap::new()),
        repos: Mutex::new(HashMap::new()),
        out: Mutex::new(io::stdout()),
        threads,
    });

    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin.lock());

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
        let method = msg.get("method").and_then(Value::as_str).unwrap_or("").to_string();
        let params = msg.get("params").cloned().unwrap_or(Value::Null);

        // git/run だけは別スレッドに渡す。読み取りループを止めないため。
        // Emacs は同期なので同時に飛んでくるのは高々 1 件だが、先読みが
        // 走っている最中に要求が来るので、ここで待つわけにはいかない。
        if method == "git/run" {
            let sh2 = sh.clone();
            thread::spawn(move || {
                let outcome = handle_run(&sh2, params);
                respond(&sh2, &id, outcome);
            });
            continue;
        }

        let outcome: Result<Value, (i32, String)> = match method.as_str() {
            "initialize" => Ok(json!({
                "protocol": PROTOCOL,
                "version": concat!("magit-gitd ", env!("CARGO_PKG_VERSION")),
                "pid": std::process::id(),
                "threads": threads,
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
                    sh.envs.lock().unwrap().insert(p.id, Arc::new(parsed));
                    Ok(json!({ "ok": true }))
                }
                Err(e) => Err((E_INVALID_PARAMS, e.to_string())),
            },

            "repo/prewarm" => match serde_json::from_value::<RepoRef>(params) {
                Ok(p) => {
                    on_prewarm(&sh, p.repo, p.token.unwrap_or(0));
                    Ok(Value::Null)
                }
                Err(e) => Err((E_INVALID_PARAMS, e.to_string())),
            },

            "repo/forget" => match serde_json::from_value::<RepoRef>(params) {
                Ok(p) => {
                    sh.repos.lock().unwrap().remove(&p.repo);
                    Ok(Value::Null)
                }
                Err(e) => Err((E_INVALID_PARAMS, e.to_string())),
            },

            "gitd/stats" => Ok(stats(&sh)),

            "shutdown" => {
                respond(&sh, &id, Ok(json!({ "ok": true })));
                break;
            }

            other => Err((E_METHOD_NOT_FOUND, format!("未知のメソッド: {other}"))),
        };

        // id が無いものは通知。応答してはいけない。
        if id.is_some() {
            respond(&sh, &id, outcome);
        }
    }
}

// ---------------------------------------------------------------- git/run

fn handle_run(sh: &Arc<Shared>, params: Value) -> Result<Value, (i32, String)> {
    let p: GitRun = serde_json::from_value(params).map_err(|e| (E_INVALID_PARAMS, e.to_string()))?;
    let want_stderr = p.want_stderr;
    let spec = Spec { program: p.program, cwd: p.cwd, args: p.args, env: p.env };
    let scope = match (p.repo.as_deref(), p.token) {
        (Some(r), Some(t)) => Some((r, t)),
        _ => None,
    };

    let (res, cached) = match (scope, p.role.as_deref()) {
        (Some((repo, token)), Some("cache")) => {
            note_token(sh, repo, token);
            record(sh, repo, &spec, false);
            cached_run(sh, repo, token, &spec)
        }
        (Some((repo, token)), Some("prelude")) => {
            note_token(sh, repo, token);
            record(sh, repo, &spec, true);
            (exec(sh, &spec), false)
        }
        (Some((repo, token)), _) => {
            // 書き込み。キャッシュしないが token は控えておく
            // (Emacs 側で既に上がっているはずなので、ここで古い分を捨てる)。
            note_token(sh, repo, token);
            (exec(sh, &spec), false)
        }
        (None, _) => (exec(sh, &spec), false),
    };

    let r = res?;
    Ok(json!({
        // Windows では常に Some。念のため。
        "exit": r.exit,
        "stdout": b64(&r.stdout),
        "stderr": if want_stderr { Value::String(b64(&r.stderr)) } else { Value::Null },
        "elapsed_ms": r.elapsed_ms,
        "cached": cached,
    }))
}

/// 実際に git を起動する。ここではキャッシュもリポジトリの状態も触らない。
fn exec(sh: &Shared, spec: &Spec) -> Outcome {
    let env = sh.envs.lock().unwrap().get(&spec.env).cloned();
    let Some(env) = env else {
        // git は起動していない。Emacs 側は登録し直して再送してよい。
        return Err((E_UNKNOWN_ENV, format!("未登録の env id: {}", spec.env)));
    };

    let t0 = Instant::now();
    let out = Command::new(&spec.program)
        .current_dir(&spec.cwd)
        .args(&spec.args)
        .env_clear()
        .envs(env.iter().cloned())
        .output();

    match out {
        Ok(o) => Ok(RunResult {
            exit: o.status.code().unwrap_or(-1),
            stdout: Arc::new(o.stdout),
            stderr: Arc::new(o.stderr),
            elapsed_ms: t0.elapsed().as_millis() as u64,
        }),
        // spawn 自体に失敗した = git は 1 度も動いていない。
        // Emacs 側はこれを見て素通しで再実行してよい (書き込みでも安全)。
        Err(e) => Err((
            E_SPAWN_FAILED,
            // cwd も入れる。起動失敗の原因はたいてい cwd の側にある
            format!("{} の起動に失敗 (cwd={}): {e}", spec.program, spec.cwd),
        )),
    }
}

// ---------------------------------------------------------------- キャッシュ

/// token が進んでいたらキャッシュを捨てる。
fn note_token(sh: &Shared, repo: &str, token: u64) {
    let mut repos = sh.repos.lock().unwrap();
    let st = repos.entry(repo.to_string()).or_default();
    if st.token != token {
        st.token = token;
        st.cache.clear();
    }
}

fn current_token(sh: &Shared, repo: &str) -> u64 {
    sh.repos.lock().unwrap().get(repo).map(|s| s.token).unwrap_or(0)
}

/// 先読みで再生するために、来たコマンドを覚えておく。
fn record(sh: &Shared, repo: &str, spec: &Spec, prelude: bool) {
    let key = spec.key();
    let mut repos = sh.repos.lock().unwrap();
    let st = repos.entry(repo.to_string()).or_default();
    let (list, cap) = if prelude {
        (&mut st.prelude, PRELUDE_CAP)
    } else {
        (&mut st.observed, RECIPE_CAP)
    };
    if list.len() < cap && !list.iter().any(|s| s.key() == key) {
        list.push(spec.clone());
    }
}

enum Step {
    Hit(RunResult),
    Follow(Arc<Job>),
    Lead(Arc<Job>),
}

/// キャッシュを見て、無ければ 1 回だけ走らせる。
/// 同じキーの実行が既に走っていればそれを待つ (single-flight)。
///
/// 戻り値の bool は「git を起動せずに済んだか」。統計にしか使わない。
fn cached_run(sh: &Arc<Shared>, repo: &str, token: u64, spec: &Spec) -> (Outcome, bool) {
    let key = spec.key();

    let step = {
        let mut repos = sh.repos.lock().unwrap();
        let st = repos.entry(repo.to_string()).or_default();
        if st.token != token {
            st.token = token;
            st.cache.clear();
        }
        if let Some(r) = st.cache.get(&key) {
            st.hits += 1;
            Step::Hit(r.clone())
        } else {
            match st.inflight.get(&key) {
                // 同じ世代の実行が走っている。待ち合わせる
                Some(j) if j.token == token => Step::Follow(j.clone()),
                _ => {
                    st.misses += 1;
                    let j = Arc::new(Job::new(token));
                    st.inflight.insert(key.clone(), j.clone());
                    Step::Lead(j)
                }
            }
        }
    };

    match step {
        Step::Hit(r) => (Ok(r), true),
        Step::Follow(j) => (j.wait(), true),
        Step::Lead(j) => {
            let res = exec(sh, spec);
            {
                let mut repos = sh.repos.lock().unwrap();
                if let Some(st) = repos.get_mut(repo) {
                    // 自分が入れた Job のときだけ外す。世代が変わって別の
                    // Job に置き換わっていることがあるため
                    if st.inflight.get(&key).is_some_and(|c| Arc::ptr_eq(c, &j)) {
                        st.inflight.remove(&key);
                    }
                    // **走らせている間に世代が変わっていたら覚えない。**
                    // 実行前と実行後で状態が違うので、この結果は信用できない
                    if st.token == token {
                        if let Ok(ref r) = res {
                            if st.cache.len() >= CACHE_CAP {
                                st.cache.clear();
                            }
                            st.cache.insert(key.clone(), r.clone());
                        }
                    }
                }
            }
            j.complete(res.clone());
            (res, false)
        }
    }
}

// ---------------------------------------------------------------- 先読み

fn on_prewarm(sh: &Arc<Shared>, repo: String, token: u64) {
    let start = {
        let mut repos = sh.repos.lock().unwrap();
        let st = repos.entry(repo.clone()).or_default();
        if st.token != token {
            st.token = token;
            st.cache.clear();
        }
        // 先読みはリフレッシュの直前に来る。つまり observed には
        // 「直前のリフレッシュで実際に使われたコマンド列」が入っている。
        // 空のときは差し替えない (先読みが 2 回続いたときに消さないため)。
        if !st.observed.is_empty() {
            st.recipe = std::mem::take(&mut st.observed);
        }
        if st.prewarming {
            st.dirty = true;
            false
        } else if st.recipe.is_empty() {
            false
        } else if st.recipe.iter().all(|s| st.cache.contains_key(&s.key())) {
            // 既に全部そろっている。**ここで打ち切らないと prelude
            // (update-index --refresh) だけが毎回走る。** それは .git と
            // .git/index.lock のイベントを出すので、Emacs 側の監視が
            // また先読みを頼み、無限に回り続ける。
            false
        } else {
            st.prewarming = true;
            st.prewarms += 1;
            true
        }
    };
    if start {
        let sh2 = sh.clone();
        thread::spawn(move || prewarm_loop(sh2, repo));
    }
}

fn prewarm_loop(sh: Arc<Shared>, repo: String) {
    for _ in 0..PREWARM_ROUNDS {
        let Some((token, prelude, recipe)) = ({
            let repos = sh.repos.lock().unwrap();
            repos.get(&repo).map(|st| (st.token, st.prelude.clone(), st.recipe.clone()))
        }) else {
            break; // repo/forget された
        };

        // prelude は直列。update-index --refresh を先に済ませておかないと、
        // このあと並列に走らせる diff 系が「stat が古いだけ」のファイルを
        // 変更ありと報告し、その答えがキャッシュに残ってしまう。
        for s in &prelude {
            if current_token(&sh, &repo) != token {
                break;
            }
            let _ = exec(&sh, s);
        }

        // レシピは並列。29 コマンドを 8 スレッドで回すと 4 波で終わる。
        let recipe = Arc::new(recipe);
        let next = Arc::new(Mutex::new(0usize));
        let n = sh.threads.min(recipe.len()).max(1);
        let mut handles = Vec::with_capacity(n);
        for _ in 0..n {
            let sh = sh.clone();
            let repo = repo.clone();
            let recipe = recipe.clone();
            let next = next.clone();
            handles.push(thread::spawn(move || loop {
                let i = {
                    let mut g = next.lock().unwrap();
                    let i = *g;
                    *g += 1;
                    i
                };
                if i >= recipe.len() || current_token(&sh, &repo) != token {
                    break;
                }
                let _ = cached_run(&sh, &repo, token, &recipe[i]);
            }));
        }
        for h in handles {
            let _ = h.join();
        }

        // 走っている間に新しい要求が来ていたらもう 1 周する。
        let again = {
            let mut repos = sh.repos.lock().unwrap();
            match repos.get_mut(&repo) {
                Some(st) if st.dirty => {
                    st.dirty = false;
                    if !st.observed.is_empty() {
                        st.recipe = std::mem::take(&mut st.observed);
                    }
                    true
                }
                _ => false,
            }
        };
        if !again {
            break;
        }
    }
    // どの抜け方をしてもフラグは必ず下ろす
    if let Some(st) = sh.repos.lock().unwrap().get_mut(&repo) {
        st.prewarming = false;
        st.dirty = false;
    }
}

// ---------------------------------------------------------------- 応答

fn stats(sh: &Shared) -> Value {
    let repos = sh.repos.lock().unwrap();
    let list: Vec<Value> = repos
        .iter()
        .map(|(root, st)| {
            json!({
                "repo": root,
                "token": st.token,
                "cached": st.cache.len(),
                "recipe": st.recipe.len(),
                "prelude": st.prelude.len(),
                "hits": st.hits,
                "misses": st.misses,
                "prewarms": st.prewarms,
            })
        })
        .collect();
    json!({ "threads": sh.threads, "repos": list })
}

fn respond(sh: &Shared, id: &Option<Value>, outcome: Result<Value, (i32, String)>) {
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
    // フレームが混ざらないよう、書き込みはここで直列化する
    let mut out = sh.out.lock().unwrap();
    if let Err(e) = write_frame(&mut *out, &body) {
        eprintln!("応答の書き込みに失敗: {e}");
    }
}
