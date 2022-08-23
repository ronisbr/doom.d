;;; ~/.doom.d/settings/setup-julia-mode.el -*- lexical-binding: t; -*-
;;
;; Configurations for Julia mode.

;; Automatically indent the code when typing `end`.
(set-electric! 'julia-mode :words '("end"))

;; Improve Julia number highlight.
(after! highlight-numbers
  (puthash 'julia-mode
           (rx (and symbol-start
                    (or
                     ;; Hexadecimal number.
                     (and "0x"
                          (+ hex-digit)
                          (0+ (and "_"
                                   (+ hex-digit))))
                     ;; Binary number.
                     (and "0b"
                          (+ (any "01"))
                          (0+ (and "_"
                                   (+ (any "01")))))
                     ;; Decimal number.
                     (and (+ digit)
                          (0+ (and "_"
                                   (+ digit)))
                          (? (and "."
                                  (* digit)
                                  (0+ (and "_" (+ digit)))))
                          (? (and (any "eE")
                                  (? (any "-+"))
                                  (+ digit)))))))
           highlight-numbers-modelist))

(provide 'setup-julia-mode)
