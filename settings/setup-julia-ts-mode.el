;;; ~/.doom.d/settings/setup-julia-mode.el -*- lexical-binding: t; -*-
;;
;; Configurations for Julia mode with tree-sitter support.

;; Configure the default environment for LSP.
(setq lsp-julia-default-environment
 (or (car (last (doom-glob "~/.julia/environments/v*")))
  "~/.julia/environments/v1.6"))

;; Enable LSP by default.
(add-hook 'julia-ts-mode-hook #'lsp-mode)
(add-hook 'julia-ts-mode-hook #'lsp)

(provide 'setup-julia-ts-mode)
