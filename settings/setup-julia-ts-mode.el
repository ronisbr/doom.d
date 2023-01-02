;;; ~/.doom.d/settings/setup-julia-ts-mode.el -*- lexical-binding: t; -*-
;;
;; Configurations for Julia mode with tree-sitter support.

;; Configure the LSP.
(use-package! lsp-julia
  :after lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(julia-ts-mode . "julia"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-julia--rls-command)
                    :major-modes '(julia-mode ess-julia-mode julia-ts-mode)
                    :server-id 'julia-ls
                    :multi-root t))
  (setq lsp-julia-default-environment
        (or (car (last (doom-glob "~/.julia/environments/v*")))
            "~/.julia/environments/v1.6")))

;; Enable LSP by default.
(add-hook 'julia-ts-mode-hook #'lsp-mode)
(add-hook 'julia-ts-mode-hook #'lsp)

(provide 'setup-julia-ts-mode)
