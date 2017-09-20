;;
;; gocode coding system
;;
(use-package company-go
  :defer t
  :config
  (defadvice company-go--invoke-autocomplete (around company-go--invoke-autocomplete-adv activate)
    (let ((old-default-process-coding-system default-process-coding-system))
      (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
      ad-do-it
    (setq default-process-coding-system old-default-process-coding-system)))
  (ad-activate-regexp "company-go--invoke-autocomplete-adv"))

(use-package go-mode
  :defer t
  :config
  (defun gofmt ()
  "Format the current buffer according to the formatting tool.

The tool used can be set via ‘gofmt-command` (default: gofmt) and additional
arguments can be set as a list via ‘gofmt-args`."
  (interactive)
  (let ((tmpfile (make-temp-file "gofmt" nil ".go"))
        (patchbuf (get-buffer-create "*Gofmt patch*"))
        (errbuf (if gofmt-show-errors (get-buffer-create "*Gofmt Errors*")))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix)
        our-gofmt-args)

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          (when (and (gofmt--is-goimports-p) buffer-file-name)
            (setq our-gofmt-args
                  (append our-gofmt-args
                          ;; srcdir, despite its name, supports
                          ;; accepting a full path, and some features
                          ;; of goimports rely on knowing the full
                          ;; name.
                          (list "-srcdir" (file-truename buffer-file-name)))))
          (setq our-gofmt-args (append our-gofmt-args
                                       gofmt-args
                                       (list "-w" tmpfile)))
          (message "Calling gofmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'call-process gofmt-command nil errbuf nil our-gofmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already gofmted")
                  (go--apply-rcs-patch patchbuf)
                  (message "Applied gofmt"))
                (if errbuf (gofmt--kill-error-buffer errbuf)))
            (message "Could not apply gofmt")
            (if errbuf (gofmt--process-errors (buffer-file-name) tmpfile errbuf))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))
  (defun godef--call (point)
  "Call godef, acquiring definition position and expression
description at POINT."
  (if (not (buffer-file-name (go--coverage-origin-buffer)))
      (error "Cannot use godef on a buffer without a file name")
    (let ((outbuf (generate-new-buffer "*godef*"))
          (coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix))
      (prog2
          (call-process-region (point-min)
                               (point-max)
                               godef-command
                               nil
                               outbuf
                               nil
                               "-i"
                               "-t"
                               "-f"
                               (file-truename (buffer-file-name (go--coverage-origin-buffer)))
                               "-o"
                               ;; Emacs point and byte positions are 1-indexed.
                               (number-to-string (1- (position-bytes point))))
          (with-current-buffer outbuf
            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
        (kill-buffer outbuf))))))
