;;; setup-cygwin.el --- Set up Emacs for using Cygwin
;;
;; Filename: setup-cygwin.el
;; Description:
;; Author: Markus Hoenika
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2012, Drew Adams, all rights reserved.
;; Created: Thu Jan 15 11:13:38 2004
;; Version: 21.0
;; Last-Updated: Sun Jan  1 17:08:55 2012 (-0800)
;;           By: dradams
;;     Update #: 106
;; URL: http://www.emacswiki.org/cgi-bin/wiki/setup-cygwin.el
;; Keywords: os, unix, cygwin
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `ange-ftp', `backquote', `comint', `cygwin-mount', `ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Set up Emacs for using Cygwin.  From Markus Hoenika's paper "SGML
;;   for Windows NT" <hoenika_markus@compuserve.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/08/11 dadams
;;     Made settings that are based on Cygwin install directory conditional, per input from Tom Popovich.
;; 2011/01/04 dadams
;;     Added autoload cookies for commands.
;; 2009/10-15 dadams
;;     Set ediff-shell to shell-file-name.
;; 2007/12/08 dadams
;;     Use absolute file name for shell-file-name.
;; 2006/11/16 dadams
;;     Replace add-to-list by setq, for Emacs -q prior to Emacs 21.
;; 2006/08/14 dadams
;;     Append, not prepend "c:/cygwin/usr/info/" to Info-default-directory-list.
;; 2004/10/01 dadams
;;     Changed Info-directory-list to Info-default-directory-list
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'undecided-unix)


;;; Add Cygwin Info pages
(setq Info-default-directory-list (append Info-default-directory-list (list "/usr/share/info/")))

;;; Use `bash' as the default shell in Emacs.
(setq explicit-shell-file-name  shell-file-name) ; Interactive shell
(setq ediff-shell               shell-file-name)    ; Ediff shell
(setq explicit-shell-args       '("--login" "-i"))

;;;;; (setq shell-command-switch "-ic") ; SHOULD THIS BE "-c" or "-ic"?
(setq w32-quote-process-args ?\") ;; " @@@ IS THIS BETTER? ;@@@ WAS THIS BEFORE: (setq w32-quote-process-args t)

;;;###autoload
(defun bash ()
  "Start `bash' shell."
  (interactive)
  (let ((binary-process-input t)
        (binary-process-output nil))
    (shell)))

(add-to-list 'process-coding-system-alist '("bash" . (raw-text-dos . raw-text-unix)))

;; From: http://www.dotfiles.com/files/6/235_.emacs
;;;###autoload
(defun set-shell-bash()
  "Enable on-the-fly switching between the bash shell and DOS."
  (interactive)
  ;; (setq binary-process-input t)
  (setq shell-file-name "bash")
  (setq shell-command-switch "-c")      ; SHOULD IT BE (setq shell-command-switch "-ic")?
  (setq explicit-shell-file-name "bash")
  (setenv "SHELL" explicit-shell-file-name)
  (setq w32-quote-process-args ?\") ;; "
  )

;;;###autoload
(defun set-shell-cmdproxy()
  "Set shell to `cmdproxy'."
  (interactive)
  (setq shell-file-name "cmdproxy")
  (setq explicit-shell-file-name "cmdproxy")
  (setenv "SHELL" explicit-shell-file-name)
  (setq w32-quote-process-args nil))

(provide 'setup-cygwin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-cygwin.el ends here
