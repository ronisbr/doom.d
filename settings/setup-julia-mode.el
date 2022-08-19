;;; ~/.doom.d/settings/setup-julia-mode.el -*- lexical-binding: t; -*-
;;
;; Configurations for Julia mode.

;; Automatically indent the code when typing `end`.
(set-electric! 'julia-mode :words '("end"))

;; In Julia, long numbers can be represented with an underscode.
;; (after! julia-mode
;;   (puthash 'julia-mode
;;            (rx (and symbol-start
;;                     (? "-")
;;                     (+ digit)
;;                     (0+ (and "_" (= 3 digit)))
;;                     symbol-end))
;;            highlight-numbers-modelist))

(provide 'setup-julia-mode)
