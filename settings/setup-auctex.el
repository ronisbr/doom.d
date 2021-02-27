;;; ~/.doom.d/settings/setup-auctex.el -*- lexical-binding: t; -*-
;;
;; Configure AUXTeX.

(setenv "PATH" (concat "/Library/TeX/texbin:"
                       (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin")

(add-hook! LaTeX-mode (setq-local display-line-numbers nil))

(provide 'setup-auctex)
