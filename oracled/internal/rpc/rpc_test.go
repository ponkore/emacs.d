package rpc

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"testing"
	"time"
)

type echoHandler struct {
	seen chan string
}

func (h *echoHandler) Handle(c *Conn, id json.RawMessage, method string,
	params json.RawMessage) (any, *Error) {
	h.seen <- method
	switch method {
	case "ok":
		return map[string]any{"method": method, "params": string(params)}, nil
	case "boom":
		return nil, Errorf(CodeInternal, "壊れた")
	case "notify-then-ok":
		c.Notify("note", map[string]any{"n": 1})
		return map[string]any{"ok": true}, nil
	}
	return nil, Errorf(CodeMethodNotFound, "未知: %s", method)
}

// frame は Content-Length 形式に包む。
func frame(body string) string {
	return fmt.Sprintf("Content-Length: %d\r\n\r\n%s", len(body), body)
}

// readFrames は書かれたものを 1 つずつ Message に戻す。
func readFrames(t *testing.T, raw string) []Message {
	t.Helper()
	var out []Message
	r := strings.NewReader(raw)
	c := NewConn(r, io.Discard)
	for {
		m, err := c.read()
		if err != nil {
			return out
		}
		out = append(out, *m)
	}
}

func TestRoundTrip(t *testing.T) {
	in := frame(`{"jsonrpc":"2.0","id":1,"method":"ok","params":{"a":1}}`) +
		frame(`{"jsonrpc":"2.0","id":2,"method":"boom","params":{}}`) +
		// 通知（id 無し）には応答しない
		frame(`{"jsonrpc":"2.0","method":"ok","params":{}}`) +
		frame(`{"jsonrpc":"2.0","id":3,"method":"notify-then-ok","params":{}}`)

	var buf bytes.Buffer
	c := NewConn(strings.NewReader(in), &buf)
	h := &echoHandler{seen: make(chan string, 10)}
	go c.Serve(h)
	<-c.Done
	// goroutine で処理しているので、4 件とも Handle を通ったことを待つ
	for i := 0; i < 4; i++ {
		select {
		case <-h.seen:
		case <-time.After(2 * time.Second):
			t.Fatalf("Handle が %d 件しか呼ばれていない", i)
		}
	}
	time.Sleep(50 * time.Millisecond) // 書き込みの落ち着き待ち

	msgs := readFrames(t, buf.String())
	var ids []string
	var methods []string
	for _, m := range msgs {
		if m.Method != "" {
			methods = append(methods, m.Method)
			continue
		}
		ids = append(ids, string(m.ID))
	}
	// 応答は id 1 / 2 / 3 の 3 件（通知には応答しない）
	if len(ids) != 3 {
		t.Errorf("応答が %d 件（期待 3）: %v", len(ids), ids)
	}
	if len(methods) != 1 || methods[0] != "note" {
		t.Errorf("通知が %v（期待 [note]）", methods)
	}
	for _, m := range msgs {
		if string(m.ID) == "2" && (m.Error == nil || m.Error.Code != CodeInternal) {
			t.Errorf("id=2 はエラーで返るべき: %+v", m)
		}
		if m.JSONRPC != "2.0" {
			t.Errorf("jsonrpc が %q", m.JSONRPC)
		}
	}
}

// 【重要】走っている文の最中に $/cancel を受け取れないと割り込みが成立しない。
// 要求ごとに goroutine を起こしていることを、塞がる Handle で確かめる。
func TestConcurrentDispatch(t *testing.T) {
	block := make(chan struct{})
	done := make(chan string, 2)
	h := handlerFunc(func(c *Conn, id json.RawMessage, method string, _ json.RawMessage) (any, *Error) {
		if method == "slow" {
			<-block // 解放されるまで戻らない
		}
		done <- method
		return map[string]any{"ok": true}, nil
	})
	in := frame(`{"jsonrpc":"2.0","id":1,"method":"slow"}`) +
		frame(`{"jsonrpc":"2.0","id":2,"method":"fast"}`)
	c := NewConn(strings.NewReader(in), io.Discard)
	go c.Serve(h)

	// slow が塞がっている間に fast が通ること
	select {
	case m := <-done:
		if m != "fast" {
			t.Fatalf("先に通ったのが %q", m)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("slow に塞がれて fast が通らない")
	}
	close(block)
	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("slow が戻らない")
	}
}

type handlerFunc func(*Conn, json.RawMessage, string, json.RawMessage) (any, *Error)

func (f handlerFunc) Handle(c *Conn, id json.RawMessage, method string,
	params json.RawMessage) (any, *Error) {
	return f(c, id, method, params)
}

func TestBadFrames(t *testing.T) {
	cases := []string{
		"Content-Length: abc\r\n\r\n{}",   // 長さが数でない
		"X: 1\r\n\r\n{}",                  // Content-Length が無い
		frame(`{"jsonrpc":"2.0","id":1,`), // JSON が壊れている
	}
	for _, in := range cases {
		c := NewConn(strings.NewReader(in), io.Discard)
		if _, err := c.read(); err == nil {
			t.Errorf("エラーになるべき: %q", in)
		}
	}
}
