;;; ~/.doom.d/settings/setup-julia-mode.el -*- lexical-binding: t; -*-
;;
;; Configurations for Julia mode.

;; Automatically indent the code when typing `end`.
(set-electric! 'julia-mode :words '("end"))

(provide 'setup-julia-mode)
