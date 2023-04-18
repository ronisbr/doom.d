;;; ~/.doom.d/settings/setup-lsp-julia.el --- Configure LSP mode for Julia language -*- lexical-binding: t; -*-
;;
;; Configure LSP for Julia language.

(setq lsp-julia-default-environment "~/.julia/environments/v1.7")

;; Indentation with LSP julia is not very good.
(setq-hook! 'julia-mode-hook +format-with-lsp nil)
