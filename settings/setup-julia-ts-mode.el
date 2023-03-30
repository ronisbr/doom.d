;;; ~/.doom.d/settings/setup-julia-ts-mode.el -*- lexical-binding: t; -*-
;;
;; Configurations for Julia mode with tree-sitter support.

(use-package! julia-ts-mode
  :mode "\\.jl$")

(after! julia-ts-mode
  ;; Enable LSP by default.
  (add-hook 'julia-ts-mode-hook #'lsp-mode)
  (add-hook 'julia-ts-mode-hook #'lsp)

  ;; Indentation with LSP julia is not very good.
  (setq-hook! 'julia-ts-mode-hook lsp-enable-indentation nil)

  ;; We do not need `highlight-numbers-mode' because everything is handled by
  ;; tree-sitter.
  (add-hook 'julia-ts-mode-hook (lambda () (highlight-numbers-mode -1)))

  ;; We must replace epsilon in Julia mode because JetBrainsMono does not have a
  ;; glyph to the lunate version of it that is used by default in Julia.
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords 'julia-ts-mode
                          '(("\\(ϵ\\)" 1 '(face nil display "ε"))))

  ;; Electric indent mode.
  (set-electric! 'julia-ts-mode
    :words '("catch"
             "else"
             "elseif"
             "finally"
             "end")))

;; Re-indent the current line when pressing return.
(map! :map julia-ts-mode-map :i [remap newline] #'reindent-then-newline-and-indent)

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
            "~/.julia/environments/v1.6"))
  (setq lsp-julia-format-indents nil)
  (setq lsp-julia-format-kw nil))

;; YAS snippet expand does not work well with tree-sitter without this
;; modification.
(setq yas-indent-line 'fixed)

(provide 'setup-julia-ts-mode)
