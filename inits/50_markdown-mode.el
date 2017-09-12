(use-package markdown-mode
  :mode (("\\.\\(markdown\\|md\\)\\.txt\\'" . markdown-mode))
  :init
  (defface markdown-inline-code-face
    '((t (:inherit (markdown-code-face font-lock-constant-face))))
    "Face for inline code."
    :group 'markdown-faces))
