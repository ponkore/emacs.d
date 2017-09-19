;;
;; gocode coding system
;;
(defadvice company-go--invoke-autocomplete (around company-go--invoke-autocomplete-adv activate)
  (let ((old-default-process-coding-system default-process-coding-system))
    (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
    ad-do-it
    (setq default-process-coding-system old-default-process-coding-system)))

(ad-activate-regexp "company-go--invoke-autocomplete-adv")
