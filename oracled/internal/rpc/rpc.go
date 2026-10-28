// Package rpc は stdio 上の JSON-RPC 2.0 を話す。
//
// フレーミングは LSP 方式（`Content-Length: N\r\n\r\n<json>`）。Emacs 側は
// 同梱の jsonrpc.el をそのまま使える（gitd と同じ流儀。設計 §4）。
//
// 【重要】読み取りは 1 本の goroutine で回し、要求ごとに別の goroutine で
// 処理する。走っている文の最中に `$/cancel` を受け取れないと、割り込みが
// 成立しない（設計 §4）。書き込みは mutex で直列化する。
package rpc

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"strconv"
	"strings"
	"sync"
)

// エラーコード（設計 §4 と gitd の流儀）
const (
	CodeParse          = -32700
	CodeInvalidRequest = -32600
	CodeMethodNotFound = -32601
	CodeInvalidParams  = -32602
	CodeInternal       = -32603
	CodeNoSession      = -32001 // 未知の session
	CodeBusy           = -32002 // その session は別の文を走らせている
	CodeCancelled      = -32003 // $/cancel で止めた
	CodeDBError        = -32010 // Oracle が返したエラー（data に詳細）
)

type Message struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Method  string          `json:"method,omitempty"`
	Params  json.RawMessage `json:"params,omitempty"`
	Result  any             `json:"result,omitempty"`
	Error   *Error          `json:"error,omitempty"`
}

type Error struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
	Data    any    `json:"data,omitempty"`
}

func (e *Error) Error() string { return fmt.Sprintf("%d: %s", e.Code, e.Message) }

func Errorf(code int, format string, args ...any) *Error {
	return &Error{Code: code, Message: fmt.Sprintf(format, args...)}
}

// Handler は 1 つの要求を処理する。notify が真なら応答は書かれない。
type Handler interface {
	Handle(c *Conn, id json.RawMessage, method string, params json.RawMessage) (any, *Error)
}

type Conn struct {
	r  *bufio.Reader
	w  io.Writer
	mu sync.Mutex // 書き込みの直列化

	// Done は stdin が EOF になったら閉じる。
	Done chan struct{}
}

func NewConn(r io.Reader, w io.Writer) *Conn {
	return &Conn{r: bufio.NewReaderSize(r, 1<<16), w: w, Done: make(chan struct{})}
}

// Serve は stdin を読み続け、要求ごとに goroutine を起こす。
// stdin が EOF になったら Done を閉じて戻る。
func (c *Conn) Serve(h Handler) {
	defer close(c.Done)
	for {
		msg, err := c.read()
		if err != nil {
			return // EOF も含めてここで終わり（Emacs が死んだときの後始末）
		}
		go func(m *Message) {
			notify := len(m.ID) == 0
			res, rerr := h.Handle(c, m.ID, m.Method, m.Params)
			if notify {
				return // 通知には応答しない
			}
			if rerr != nil {
				c.Write(&Message{JSONRPC: "2.0", ID: m.ID, Error: rerr})
				return
			}
			c.Write(&Message{JSONRPC: "2.0", ID: m.ID, Result: res})
		}(msg)
	}
}

// Notify はサーバ発の通知を送る（rows/chunk など）。
func (c *Conn) Notify(method string, params any) {
	c.Write(&Message{JSONRPC: "2.0", Method: method, Result: nil, Params: mustRaw(params)})
}

func mustRaw(v any) json.RawMessage {
	b, err := json.Marshal(v)
	if err != nil {
		b, _ = json.Marshal(map[string]string{"error": err.Error()})
	}
	return b
}

func (c *Conn) Write(m *Message) {
	if m.JSONRPC == "" {
		m.JSONRPC = "2.0"
	}
	body, err := json.Marshal(m)
	if err != nil {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	fmt.Fprintf(c.w, "Content-Length: %d\r\n\r\n", len(body))
	c.w.Write(body)
	if f, ok := c.w.(interface{ Flush() error }); ok {
		f.Flush()
	}
}

func (c *Conn) read() (*Message, error) {
	length := -1
	for {
		line, err := c.r.ReadString('\n')
		if err != nil {
			return nil, err
		}
		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			break // ヘッダの終わり
		}
		if k, v, ok := strings.Cut(line, ":"); ok &&
			strings.EqualFold(strings.TrimSpace(k), "Content-Length") {
			length, err = strconv.Atoi(strings.TrimSpace(v))
			if err != nil {
				return nil, fmt.Errorf("Content-Length が読めない: %w", err)
			}
		}
	}
	if length < 0 {
		return nil, fmt.Errorf("Content-Length が無い")
	}
	buf := make([]byte, length)
	if _, err := io.ReadFull(c.r, buf); err != nil {
		return nil, err
	}
	var m Message
	if err := json.Unmarshal(buf, &m); err != nil {
		return nil, err
	}
	return &m, nil
}
