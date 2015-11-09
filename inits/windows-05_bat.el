(require 'bat-mode)
(setq bat-font-lock-keywords
  (eval-when-compile
    (let ((COMMANDS
           '("assoc" "at" "attrib" "cd" "cls" "color" "copy" "date" "del" "dir"
             "doskey" "echo" "endlocal" "erase" "fc" "find" "findstr" "format"
             "ftype" "label" "md" "mkdir" "more" "move" "net" "path" "pause"
             "popd" "prompt" "pushd" "rd" "ren" "rename" "replace" "rmdir" "set"
             "setlocal" "shift" "sort" "subst" "time" "title" "tree" "type"
             "ver" "vol" "xcopy"))
          (CONTROLFLOW
           '("call" "cmd" "defined" "do" "else" "equ" "exist" "exit" "for" "geq"
             "goto" "gtr" "if" "in" "leq" "lss" "neq" "not" "start"))
          (UNIX
           '("bash" "cat" "cp" "fgrep" "grep" "ls" "sed" "sh" "mv" "rm")))
      `(("\\_<\\(call\\|goto\\)\\_>[ \t]+%?\\([A-Za-z0-9-_\\:.]+\\)%?"
         (2 font-lock-constant-face t))
        ("^:[^:].*"
         . 'bat-label-face)
        ("\\_<\\(defined\\|set\\)\\_>[ \t]*\\(\\w+\\)"
         (2 font-lock-variable-name-face))
        ("%\\([A-Za-z0-9_]+\\)%?"
         (1 font-lock-variable-name-face))
        ("!\\([A-Za-z0-9_]+\\)!?"        ; delayed-expansion !variable!
         (1 font-lock-variable-name-face))
        ("[ =][-/]+\\([A-Za-z0-9_]\\)"
         (1 font-lock-type-face append))
        (,(concat "\\_<" (regexp-opt COMMANDS) "\\_>") . font-lock-builtin-face)
        (,(concat "\\_<" (regexp-opt CONTROLFLOW) "\\_>") . font-lock-keyword-face)
        (,(concat "\\_<" (regexp-opt UNIX) "\\_>") . font-lock-warning-face)))))
