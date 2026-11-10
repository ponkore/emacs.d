/* hwnd-probe.c --- Emacs のフレームに子 HWND を貼れるかを確かめるプローブ
 *
 * 調べたいこと:
 *   1. (frame-parameter nil 'window-id) の HWND を親にして、別スレッドで
 *      WS_CHILD のウィンドウを作れるか
 *   2. window-inside-pixel-edges をそのままクライアント座標として使えるか
 *   3. Emacs の再描画が子 HWND を塗りつぶさないか (WS_CLIPCHILDREN)
 *   4. 子フレーム (posframe / corfu) との重なり順
 *   5. クリックでフォーカスを奪わないか (WS_EX_NOACTIVATE / MA_NOACTIVATE)
 *   6. 子 HWND のスレッドが詰まったとき Emacs の入力が止まるか
 *      (親子がスレッドをまたぐと入力キューが連結される)
 *
 * 子 HWND はすべて module が立てた UI スレッドが持つ。Lisp スレッドからの
 * 操作は SendMessageTimeout で UI スレッドに頼むので、UI スレッドが詰まって
 * いても Emacs は固まらない (タイムアウトで nil が返る)。
 */
#define _WIN32_WINNT 0x0A00
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "emacs-module.h"

int plugin_is_GPL_compatible;

extern IMAGE_DOS_HEADER __ImageBase;
#define HINST ((HINSTANCE) &__ImageBase)

enum {
  WM_P_CREATE = WM_APP + 1,     /* スレッドメッセージ。lParam = struct create_req * */
  WM_P_BLOCK,                   /* スレッドメッセージ。wParam = ミリ秒 */
  WM_P_MOVE,                    /* 以下はウィンドウメッセージ */
  WM_P_SHOW,
  WM_P_ZORDER,
  WM_P_DESTROY,
  WM_P_FOCUS,
};

#define MAX_VIEWS 16

struct view {
  HWND hwnd;
  COLORREF color;
  char label[64];
  volatile LONG paints, clicks, mouseact, setfocus;
  volatile LONG wheel, wheel_delta, cap_moves, cap_outside;
};

struct create_req {
  HWND parent;
  struct view *view;
  HWND result;
  HANDLE done;
};

static struct view views[MAX_VIEWS];
static int nviews;
static DWORD ui_tid;
static HANDLE ui_ready;

static LRESULT CALLBACK
view_proc (HWND hwnd, UINT msg, WPARAM wp, LPARAM lp)
{
  struct view *v = (struct view *) GetWindowLongPtrA (hwnd, GWLP_USERDATA);
  switch (msg)
    {
    case WM_NCCREATE:
      {
        CREATESTRUCTA *cs = (CREATESTRUCTA *) lp;
        SetWindowLongPtrA (hwnd, GWLP_USERDATA, (LONG_PTR) cs->lpCreateParams);
        break;
      }
    case WM_ERASEBKGND:
      return 1;
    case WM_PAINT:
      {
        PAINTSTRUCT ps;
        HDC dc = BeginPaint (hwnd, &ps);
        RECT rc;
        GetClientRect (hwnd, &rc);
        HBRUSH br = CreateSolidBrush (v->color);
        FillRect (dc, &rc, br);
        DeleteObject (br);
        LONG n = InterlockedIncrement (&v->paints);
        char buf[160];
        snprintf (buf, sizeof buf, "%s\npaint #%ld  %ldx%ld",
                  v->label, n, rc.right, rc.bottom);
        SetBkMode (dc, TRANSPARENT);
        SetTextColor (dc, RGB (255, 255, 255));
        DrawTextA (dc, buf, -1, &rc, DT_CENTER | DT_VCENTER | DT_NOCLIP);
        EndPaint (hwnd, &ps);
        return 0;
      }
    case WM_MOUSEACTIVATE:
      InterlockedIncrement (&v->mouseact);
      return MA_NOACTIVATE;
    case WM_LBUTTONDOWN:
      InterlockedIncrement (&v->clicks);
      SetCapture (hwnd);
      return 0;
    case WM_LBUTTONUP:
      if (GetCapture () == hwnd)
        ReleaseCapture ();
      return 0;
    case WM_MOUSEMOVE:
      if (GetCapture () == hwnd)
        {
          RECT rc;
          GetClientRect (hwnd, &rc);
          POINT p = { (short) LOWORD (lp), (short) HIWORD (lp) };
          InterlockedIncrement (&v->cap_moves);
          if (!PtInRect (&rc, p))
            InterlockedIncrement (&v->cap_outside);
        }
      return 0;
    case WM_MOUSEWHEEL:
      InterlockedIncrement (&v->wheel);
      InterlockedExchangeAdd (&v->wheel_delta, GET_WHEEL_DELTA_WPARAM (wp));
      return 0;
    case WM_SETFOCUS:
      InterlockedIncrement (&v->setfocus);
      break;
    case WM_P_MOVE:
      {
        int x = (short) LOWORD (wp), y = (short) HIWORD (wp);
        int w = LOWORD (lp), h = HIWORD (lp);
        SetWindowPos (hwnd, NULL, x, y, w, h, SWP_NOZORDER | SWP_NOACTIVATE);
        InvalidateRect (hwnd, NULL, FALSE);
        return 1;
      }
    case WM_P_SHOW:
      ShowWindow (hwnd, wp ? SW_SHOWNA : SW_HIDE);
      return 1;
    case WM_P_ZORDER:
      SetWindowPos (hwnd, wp ? HWND_TOP : HWND_BOTTOM, 0, 0, 0, 0,
                    SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);
      return 1;
    case WM_P_DESTROY:
      DestroyWindow (hwnd);
      return 1;
    case WM_P_FOCUS:
      {
        /* 入力キューが連結されていれば、このスレッドの GetFocus は
           Emacs のフレームを返す。 */
        HWND f = GetFocus ();
        HWND fg = GetForegroundWindow ();
        int r = f == hwnd ? 2 : f == GetParent (hwnd) ? 1 : f ? 3 : 0;
        if (fg == GetAncestor (hwnd, GA_ROOT))
          r |= 16;
        return r;
      }
    }
  return DefWindowProcA (hwnd, msg, wp, lp);
}

static DWORD WINAPI
ui_main (LPVOID arg)
{
  (void) arg;
  MSG msg;
  PeekMessageA (&msg, NULL, WM_USER, WM_USER, PM_NOREMOVE);
  WNDCLASSA wc = {0};
  wc.lpfnWndProc = view_proc;
  wc.hInstance = HINST;
  wc.hCursor = LoadCursor (NULL, IDC_ARROW);
  wc.lpszClassName = "HwndProbeView";
  RegisterClassA (&wc);
  SetEvent (ui_ready);
  while (GetMessageA (&msg, NULL, 0, 0) > 0)
    {
      if (msg.hwnd == NULL && msg.message == WM_P_CREATE)
        {
          struct create_req *r = (struct create_req *) msg.lParam;
          r->result = CreateWindowExA (WS_EX_NOPARENTNOTIFY | WS_EX_NOACTIVATE,
                                       "HwndProbeView", r->view->label,
                                       WS_CHILD | WS_CLIPSIBLINGS,
                                       0, 0, 10, 10, r->parent, NULL, HINST,
                                       r->view);
          SetEvent (r->done);
          continue;
        }
      if (msg.hwnd == NULL && msg.message == WM_P_BLOCK)
        {
          Sleep ((DWORD) msg.wParam);
          continue;
        }
      TranslateMessage (&msg);
      DispatchMessageA (&msg);
    }
  return 0;
}

/* ---------- Emacs 側の小道具 ---------- */

static emacs_value
nil (emacs_env *env)
{
  return env->intern (env, "nil");
}

static emacs_value
tee (emacs_env *env)
{
  return env->intern (env, "t");
}

static emacs_value
bool_val (emacs_env *env, int b)
{
  return b ? tee (env) : nil (env);
}

static emacs_value
list (emacs_env *env, ptrdiff_t n, emacs_value *a)
{
  return env->funcall (env, env->intern (env, "list"), n, a);
}

static HWND
arg_hwnd (emacs_env *env, emacs_value v)
{
  return (HWND) (uintptr_t) env->extract_integer (env, v);
}

static struct view *
arg_view (emacs_env *env, emacs_value v)
{
  intmax_t i = env->extract_integer (env, v);
  if (i < 0 || i >= nviews || !views[i].hwnd)
    return NULL;
  return &views[i];
}

/* UI スレッドへの依頼。詰まっていたら 2 秒で諦めて -1 を返す。 */
static LRESULT
ui_send (HWND hwnd, UINT msg, WPARAM wp, LPARAM lp)
{
  DWORD_PTR res = 0;
  if (!SendMessageTimeoutA (hwnd, msg, wp, lp, SMTO_ABORTIFHUNG | SMTO_BLOCK,
                            2000, &res))
    return -1;
  return (LRESULT) res;
}

static COLORREF
parse_color (emacs_env *env, emacs_value v)
{
  char buf[16];
  ptrdiff_t len = sizeof buf;
  if (!env->copy_string_contents (env, v, buf, &len) || buf[0] != '#')
    return RGB (128, 128, 128);
  unsigned long rgb = strtoul (buf + 1, NULL, 16);
  return RGB ((rgb >> 16) & 255, (rgb >> 8) & 255, rgb & 255);
}

/* ---------- 公開する関数 ---------- */

/* (hwnd-probe-create PARENT COLOR LABEL) => ID */
static emacs_value
F_create (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  if (!ui_tid)
    {
      ui_ready = CreateEventA (NULL, TRUE, FALSE, NULL);
      CreateThread (NULL, 0, ui_main, NULL, 0, &ui_tid);
      WaitForSingleObject (ui_ready, 5000);
    }
  if (nviews >= MAX_VIEWS)
    return nil (env);
  struct view *v = &views[nviews];
  memset (v, 0, sizeof *v);
  v->color = parse_color (env, a[1]);
  ptrdiff_t len = sizeof v->label;
  env->copy_string_contents (env, a[2], v->label, &len);
  struct create_req r = { arg_hwnd (env, a[0]), v, NULL,
                          CreateEventA (NULL, TRUE, FALSE, NULL) };
  PostThreadMessageA (ui_tid, WM_P_CREATE, 0, (LPARAM) &r);
  DWORD w = WaitForSingleObject (r.done, 5000);
  CloseHandle (r.done);
  if (w != WAIT_OBJECT_0 || !r.result)
    return nil (env);
  v->hwnd = r.result;
  return env->make_integer (env, nviews++);
}

/* (hwnd-probe-move ID X Y W H) => t / nil (タイムアウト) */
static emacs_value
F_move (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  if (!v)
    return nil (env);
  int x = env->extract_integer (env, a[1]), y = env->extract_integer (env, a[2]);
  int w = env->extract_integer (env, a[3]), h = env->extract_integer (env, a[4]);
  WPARAM wp = (WPARAM) (((unsigned) y & 0xffff) << 16 | ((unsigned) x & 0xffff));
  LPARAM lp = (LPARAM) (((unsigned) h & 0xffff) << 16 | ((unsigned) w & 0xffff));
  return bool_val (env, ui_send (v->hwnd, WM_P_MOVE, wp, lp) == 1);
}

/* (hwnd-probe-show ID FLAG) */
static emacs_value
F_show (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  if (!v)
    return nil (env);
  return bool_val (env, ui_send (v->hwnd, WM_P_SHOW, env->is_not_nil (env, a[1]), 0) == 1);
}

/* (hwnd-probe-zorder ID TOP-P) */
static emacs_value
F_zorder (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  if (!v)
    return nil (env);
  return bool_val (env, ui_send (v->hwnd, WM_P_ZORDER, env->is_not_nil (env, a[1]), 0) == 1);
}

/* (hwnd-probe-destroy ID) */
static emacs_value
F_destroy (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  if (!v)
    return nil (env);
  int ok = ui_send (v->hwnd, WM_P_DESTROY, 0, 0) == 1;
  if (ok)
    v->hwnd = NULL;
  return bool_val (env, ok);
}

/* (hwnd-probe-info ID) => (PAINTS CLICKS MOUSEACTIVATE SETFOCUS) */
static emacs_value
F_info (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  if (!v)
    return nil (env);
  emacs_value x[8] = {
    env->make_integer (env, v->paints), env->make_integer (env, v->clicks),
    env->make_integer (env, v->mouseact), env->make_integer (env, v->setfocus),
    env->make_integer (env, v->wheel), env->make_integer (env, v->wheel_delta),
    env->make_integer (env, v->cap_moves), env->make_integer (env, v->cap_outside) };
  return list (env, 8, x);
}

/* (hwnd-probe-focus ID) => 整数。下位 4 ビット: 0=なし 1=Emacs のフレーム
   2=この子 HWND 3=その他。16 が立っていれば Emacs が前面。-1 は UI スレッド
   が応答しない。 */
static emacs_value
F_focus (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  if (!v)
    return nil (env);
  return env->make_integer (env, ui_send (v->hwnd, WM_P_FOCUS, 0, 0));
}

/* (hwnd-probe-block MS) UI スレッドを MS ミリ秒眠らせる (詰まりの再現) */
static emacs_value
F_block (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  if (!ui_tid)
    return nil (env);
  return bool_val (env, PostThreadMessageA (ui_tid, WM_P_BLOCK,
                                            (WPARAM) env->extract_integer (env, a[0]), 0));
}

/* Emacs (system-aware) のクライアント座標を、画面の物理ピクセルに直す。
   150% のモニタでは DWM が引き伸ばしているので、論理座標のまま
   GetPixel すると実際の表示とずれる。 */
static POINT
client_to_physical (HWND parent, POINT p)
{
  ClientToScreen (parent, &p);
  LogicalToPhysicalPointForPerMonitorDPI (parent, &p);
  return p;
}

/* (hwnd-probe-pixel PARENT X Y) => "#rrggbb"。画面に実際に出ている色
   (X Y は Emacs のクライアント座標。物理ピクセルに直して読む) */
static emacs_value
F_pixel (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND parent = arg_hwnd (env, a[0]);
  POINT p = { (LONG) env->extract_integer (env, a[1]),
              (LONG) env->extract_integer (env, a[2]) };
  p = client_to_physical (parent, p);
  DPI_AWARENESS_CONTEXT old
    = SetThreadDpiAwarenessContext (DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
  HDC dc = GetDC (NULL);
  COLORREF c = GetPixel (dc, p.x, p.y);
  ReleaseDC (NULL, dc);
  SetThreadDpiAwarenessContext (old);
  char buf[16];
  snprintf (buf, sizeof buf, "#%02x%02x%02x",
            GetRValue (c), GetGValue (c), GetBValue (c));
  return env->make_string (env, buf, strlen (buf));
}

/* (hwnd-probe-capture PARENT FILE) クライアント領域を画面から BMP に保存 */
static emacs_value
F_capture (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND parent = arg_hwnd (env, a[0]);
  char path[MAX_PATH];
  ptrdiff_t len = sizeof path;
  if (!env->copy_string_contents (env, a[1], path, &len))
    return nil (env);
  RECT rc;
  GetClientRect (parent, &rc);
  POINT o = client_to_physical (parent, (POINT) {0, 0});
  POINT e = client_to_physical (parent, (POINT) {rc.right, rc.bottom});
  int w = e.x - o.x, h = e.y - o.y;
  DPI_AWARENESS_CONTEXT oldctx
    = SetThreadDpiAwarenessContext (DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
  HDC sdc = GetDC (NULL);
  HDC mdc = CreateCompatibleDC (sdc);
  BITMAPINFO bi = {0};
  bi.bmiHeader.biSize = sizeof (BITMAPINFOHEADER);
  bi.bmiHeader.biWidth = w;
  bi.bmiHeader.biHeight = h;
  bi.bmiHeader.biPlanes = 1;
  bi.bmiHeader.biBitCount = 32;
  bi.bmiHeader.biCompression = BI_RGB;
  void *bits;
  HBITMAP bmp = CreateDIBSection (sdc, &bi, DIB_RGB_COLORS, &bits, NULL, 0);
  HGDIOBJ old = SelectObject (mdc, bmp);
  BitBlt (mdc, 0, 0, w, h, sdc, o.x, o.y, SRCCOPY | CAPTUREBLT);
  GdiFlush ();
  int ok = 0;
  FILE *f = fopen (path, "wb");
  if (f)
    {
      DWORD size = (DWORD) w * h * 4;
      BITMAPFILEHEADER fh = {0};
      fh.bfType = 0x4d42;
      fh.bfOffBits = sizeof fh + sizeof (BITMAPINFOHEADER);
      fh.bfSize = fh.bfOffBits + size;
      fwrite (&fh, sizeof fh, 1, f);
      fwrite (&bi.bmiHeader, sizeof (BITMAPINFOHEADER), 1, f);
      fwrite (bits, size, 1, f);
      fclose (f);
      ok = 1;
    }
  SelectObject (mdc, old);
  DeleteObject (bmp);
  DeleteDC (mdc);
  ReleaseDC (NULL, sdc);
  SetThreadDpiAwarenessContext (oldctx);
  return bool_val (env, ok);
}

/* 子 HWND の中央が画面上でこの子 HWND かを確かめ、中央の座標を返す */
static int
view_center (struct view *v, POINT *c)
{
  RECT r;
  GetWindowRect (v->hwnd, &r);
  c->x = (r.left + r.right) / 2;
  c->y = (r.top + r.bottom) / 2;
  return WindowFromPoint (*c) == v->hwnd;
}

/* (hwnd-probe-wheel ID NOTCHES) 中央で本物のホイールを回す (正 = 下へ) */
static emacs_value
F_wheel (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  POINT c, saved;
  if (!v || !view_center (v, &c))
    return nil (env);
  int notches = env->extract_integer (env, a[1]);
  GetCursorPos (&saved);
  SetCursorPos (c.x, c.y);
  Sleep (30);
  for (int i = 0; i < (notches < 0 ? -notches : notches); i++)
    {
      INPUT in = {0};
      in.type = INPUT_MOUSE;
      in.mi.dwFlags = MOUSEEVENTF_WHEEL;
      in.mi.mouseData = (DWORD) (notches < 0 ? WHEEL_DELTA : -WHEEL_DELTA);
      SendInput (1, &in, sizeof in);
      Sleep (20);
    }
  Sleep (80);
  SetCursorPos (saved.x, saved.y);
  return tee (env);
}

/* (hwnd-probe-drag ID DX DY) 中央を押したまま (DX, DY) まで動かして離す */
static emacs_value
F_drag (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  POINT c, saved;
  if (!v || !view_center (v, &c))
    return nil (env);
  int dx = env->extract_integer (env, a[1]), dy = env->extract_integer (env, a[2]);
  GetCursorPos (&saved);
  SetCursorPos (c.x, c.y);
  Sleep (30);
  INPUT in = {0};
  in.type = INPUT_MOUSE;
  in.mi.dwFlags = MOUSEEVENTF_LEFTDOWN;
  SendInput (1, &in, sizeof in);
  Sleep (30);
  for (int i = 1; i <= 10; i++)
    {
      SetCursorPos (c.x + dx * i / 10, c.y + dy * i / 10);
      Sleep (20);
    }
  in.mi.dwFlags = MOUSEEVENTF_LEFTUP;
  SendInput (1, &in, sizeof in);
  Sleep (80);
  SetCursorPos (saved.x, saved.y);
  return tee (env);
}

/* PARENT のクライアント座標 (X, Y) の一番上が PARENT の子孫かを確かめ、画面座標を返す。
   よそのアプリを押さないための安全確認 */
static int
safe_point (HWND parent, int x, int y, POINT *out, HWND *hit)
{
  POINT p = { x, y };
  ClientToScreen (parent, &p);
  HWND w = WindowFromPoint (p);
  *out = p;
  *hit = w;
  return w && (w == parent || IsChild (parent, w));
}

/* (hwnd-probe-click-at PARENT X Y CTRL) その点を本物の入力でクリックする。
   => 押した HWND のクラス名 / nil (よその窓なので押さなかった) */
static emacs_value
F_click_at (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND parent = arg_hwnd (env, a[0]);
  POINT p, saved;
  HWND hit;
  if (!safe_point (parent, env->extract_integer (env, a[1]),
                   env->extract_integer (env, a[2]), &p, &hit))
    return nil (env);
  int ctrl = env->is_not_nil (env, a[3]);
  GetCursorPos (&saved);
  SetCursorPos (p.x, p.y);
  Sleep (30);
  INPUT in[4] = {0};
  int k = 0;
  if (ctrl)
    {
      in[k].type = INPUT_KEYBOARD;
      in[k++].ki.wVk = VK_CONTROL;
    }
  in[k].type = INPUT_MOUSE;
  in[k++].mi.dwFlags = MOUSEEVENTF_LEFTDOWN;
  in[k].type = INPUT_MOUSE;
  in[k++].mi.dwFlags = MOUSEEVENTF_LEFTUP;
  if (ctrl)
    {
      in[k].type = INPUT_KEYBOARD;
      in[k].ki.wVk = VK_CONTROL;
      in[k++].ki.dwFlags = KEYEVENTF_KEYUP;
    }
  SendInput (k, in, sizeof (INPUT));
  Sleep (80);
  SetCursorPos (saved.x, saved.y);
  char cls[64] = "?";
  GetClassNameA (hit, cls, sizeof cls);
  return env->make_string (env, cls, strlen (cls));
}

/* (hwnd-probe-wheel-at PARENT X Y NOTCHES) その点で本物のホイールを回す (正 = 下へ) */
static emacs_value
F_wheel_at (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND parent = arg_hwnd (env, a[0]);
  POINT p, saved;
  HWND hit;
  if (!safe_point (parent, env->extract_integer (env, a[1]),
                   env->extract_integer (env, a[2]), &p, &hit))
    return nil (env);
  int notches = env->extract_integer (env, a[3]);
  GetCursorPos (&saved);
  SetCursorPos (p.x, p.y);
  Sleep (30);
  for (int i = 0; i < (notches < 0 ? -notches : notches); i++)
    {
      INPUT in = {0};
      in.type = INPUT_MOUSE;
      in.mi.dwFlags = MOUSEEVENTF_WHEEL;
      in.mi.mouseData = (DWORD) (notches < 0 ? WHEEL_DELTA : -WHEEL_DELTA);
      SendInput (1, &in, sizeof in);
      Sleep (20);
    }
  Sleep (80);
  SetCursorPos (saved.x, saved.y);
  char cls[64] = "?";
  GetClassNameA (hit, cls, sizeof cls);
  return env->make_string (env, cls, strlen (cls));
}

/* (hwnd-probe-monitors) => ((LX LY LW LH PX PY PW PH DPI PRIMARY-P) ...)
   L* は Emacs (system-aware) から見た座標、P* は物理ピクセル */
struct mon { HMONITOR h; RECT l, p; UINT dpi; int primary; };
static struct mon mons[8];
static int nmons;

static BOOL CALLBACK
mon_cb (HMONITOR m, HDC dc, LPRECT r, LPARAM lp)
{
  (void) dc; (void) r;
  int phys = (int) lp;
  MONITORINFO mi = { sizeof mi };
  GetMonitorInfoA (m, &mi);
  if (!phys)
    {
      if (nmons >= 8)
        return FALSE;
      mons[nmons].h = m;
      mons[nmons].l = mi.rcMonitor;
      mons[nmons].primary = (mi.dwFlags & MONITORINFOF_PRIMARY) != 0;
      UINT dx = 0, dy = 0;
      HMODULE sh = LoadLibraryA ("shcore.dll");
      typedef HRESULT (WINAPI *gdfm_t) (HMONITOR, int, UINT *, UINT *);
      gdfm_t gdfm = sh ? (gdfm_t) (void *) GetProcAddress (sh, "GetDpiForMonitor") : NULL;
      if (gdfm)
        gdfm (m, 0 /* MDT_EFFECTIVE_DPI */, &dx, &dy);
      mons[nmons].dpi = dx;
      nmons++;
    }
  else
    {
      for (int i = 0; i < nmons; i++)
        if (mons[i].h == m)
          mons[i].p = mi.rcMonitor;
    }
  return TRUE;
}

static emacs_value
F_monitors (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d; (void) a;
  nmons = 0;
  memset (mons, 0, sizeof mons);
  EnumDisplayMonitors (NULL, NULL, mon_cb, 0);
  /* 物理座標は per-monitor aware に切り替えて取り直す (HMONITOR で突き合わせる) */
  DPI_AWARENESS_CONTEXT old
    = SetThreadDpiAwarenessContext (DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
  EnumDisplayMonitors (NULL, NULL, mon_cb, 1);
  SetThreadDpiAwarenessContext (old);
  emacs_value items[8];
  for (int i = 0; i < nmons; i++)
    {
      struct mon *m = &mons[i];
      emacs_value x[10] = {
        env->make_integer (env, m->l.left), env->make_integer (env, m->l.top),
        env->make_integer (env, m->l.right - m->l.left),
        env->make_integer (env, m->l.bottom - m->l.top),
        env->make_integer (env, m->p.left), env->make_integer (env, m->p.top),
        env->make_integer (env, m->p.right - m->p.left),
        env->make_integer (env, m->p.bottom - m->p.top),
        env->make_integer (env, m->dpi), bool_val (env, m->primary) };
      items[i] = list (env, 10, x);
    }
  return list (env, nmons, items);
}

/* (hwnd-probe-click ID) 子 HWND の中央を本物の入力でクリックする。
   その点の一番上にいるのがこの子 HWND のときだけ押す (よそのアプリを
   押さないため)。=> clicked / 覆っている HWND のクラス名 */
static emacs_value
F_click (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  struct view *v = arg_view (env, a[0]);
  if (!v)
    return nil (env);
  RECT r;
  GetWindowRect (v->hwnd, &r);
  POINT c = { (r.left + r.right) / 2, (r.top + r.bottom) / 2 };
  HWND top = WindowFromPoint (c);
  if (top != v->hwnd)
    {
      char cls[64] = "?";
      if (top)
        GetClassNameA (top, cls, sizeof cls);
      return env->make_string (env, cls, strlen (cls));
    }
  POINT saved;
  GetCursorPos (&saved);
  SetCursorPos (c.x, c.y);
  INPUT in[2] = {0};
  in[0].type = in[1].type = INPUT_MOUSE;
  in[0].mi.dwFlags = MOUSEEVENTF_LEFTDOWN;
  in[1].mi.dwFlags = MOUSEEVENTF_LEFTUP;
  SendInput (2, in, sizeof (INPUT));
  Sleep (80);
  SetCursorPos (saved.x, saved.y);
  return env->intern (env, "clicked");
}

/* (hwnd-probe-key PARENT) Emacs が前面のときだけ F24 を本物の入力で送る */
static emacs_value
F_key (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND parent = arg_hwnd (env, a[0]);
  if (GetForegroundWindow () != GetAncestor (parent, GA_ROOT))
    return nil (env);
  INPUT in[2] = {0};
  in[0].type = in[1].type = INPUT_KEYBOARD;
  in[0].ki.wVk = in[1].ki.wVk = VK_F24;
  in[1].ki.dwFlags = KEYEVENTF_KEYUP;
  SendInput (2, in, sizeof (INPUT));
  return tee (env);
}

/* (hwnd-probe-ping PARENT) Emacs の GUI スレッドが応答するまでのミリ秒。
   3 秒で応答しなければ -1 */
static emacs_value
F_ping (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND parent = arg_hwnd (env, a[0]);
  LARGE_INTEGER f, t0, t1;
  QueryPerformanceFrequency (&f);
  QueryPerformanceCounter (&t0);
  DWORD_PTR res;
  BOOL ok = SendMessageTimeoutA (parent, WM_NULL, 0, 0, SMTO_ABORTIFHUNG, 3000, &res);
  QueryPerformanceCounter (&t1);
  if (!ok)
    return env->make_integer (env, -1);
  return env->make_float (env, (double) (t1.QuadPart - t0.QuadPart) * 1000.0 / f.QuadPart);
}

/* (hwnd-probe-topmost PARENT FLAG) 測定中だけフレームを最前面に置く
   (GetPixel は画面に出ている色を読むので、隠れていると測れない) */
static emacs_value
F_topmost (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND root = GetAncestor (arg_hwnd (env, a[0]), GA_ROOT);
  return bool_val (env, SetWindowPos (root, env->is_not_nil (env, a[1])
                                      ? HWND_TOPMOST : HWND_NOTOPMOST,
                                      0, 0, 0, 0,
                                      SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE));
}

/* (hwnd-probe-env PARENT) => (DPI-AWARENESS DPI ROOT=PARENT-P CLIENT-W CLIENT-H
                               SCREEN-X SCREEN-Y) */
static emacs_value
F_env (emacs_env *env, ptrdiff_t n, emacs_value *a, void *d)
{
  (void) n; (void) d;
  HWND parent = arg_hwnd (env, a[0]);
  int aw = (int) GetAwarenessFromDpiAwarenessContext (GetThreadDpiAwarenessContext ());
  RECT rc;
  GetClientRect (parent, &rc);
  POINT o = {0, 0};
  ClientToScreen (parent, &o);
  emacs_value x[7] = {
    env->make_integer (env, aw),
    env->make_integer (env, GetDpiForWindow (parent)),
    bool_val (env, GetAncestor (parent, GA_ROOT) == parent),
    env->make_integer (env, rc.right), env->make_integer (env, rc.bottom),
    env->make_integer (env, o.x), env->make_integer (env, o.y) };
  return list (env, 7, x);
}

static void
defun (emacs_env *env, const char *name, ptrdiff_t arity,
       emacs_value (*fn) (emacs_env *, ptrdiff_t, emacs_value *, void *))
{
  emacs_value f = env->make_function (env, arity, arity, fn, NULL, NULL);
  emacs_value args[2] = { env->intern (env, name), f };
  env->funcall (env, env->intern (env, "defalias"), 2, args);
}

int
emacs_module_init (struct emacs_runtime *rt)
{
  emacs_env *env = rt->get_environment (rt);
  defun (env, "hwnd-probe-create", 3, F_create);
  defun (env, "hwnd-probe-move", 5, F_move);
  defun (env, "hwnd-probe-show", 2, F_show);
  defun (env, "hwnd-probe-zorder", 2, F_zorder);
  defun (env, "hwnd-probe-destroy", 1, F_destroy);
  defun (env, "hwnd-probe-info", 1, F_info);
  defun (env, "hwnd-probe-focus", 1, F_focus);
  defun (env, "hwnd-probe-block", 1, F_block);
  defun (env, "hwnd-probe-pixel", 3, F_pixel);
  defun (env, "hwnd-probe-capture", 2, F_capture);
  defun (env, "hwnd-probe-click", 1, F_click);
  defun (env, "hwnd-probe-key", 1, F_key);
  defun (env, "hwnd-probe-ping", 1, F_ping);
  defun (env, "hwnd-probe-topmost", 2, F_topmost);
  defun (env, "hwnd-probe-env", 1, F_env);
  defun (env, "hwnd-probe-wheel", 2, F_wheel);
  defun (env, "hwnd-probe-drag", 3, F_drag);
  defun (env, "hwnd-probe-monitors", 0, F_monitors);
  defun (env, "hwnd-probe-click-at", 4, F_click_at);
  defun (env, "hwnd-probe-wheel-at", 4, F_wheel_at);
  emacs_value p = env->intern (env, "hwnd-probe");
  env->funcall (env, env->intern (env, "provide"), 1, &p);
  return 0;
}
