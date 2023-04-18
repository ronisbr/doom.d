;;; ~/.doom.d/settings/setup-julia-ts-mode.el --- Configure julia-ts-mode -*- lexical-binding: t; -*-

(use-package! julia-ts-mode
  :mode "\\.jl$")

(after! julia-ts-mode
  ;; We do not need `highlight-numbers-mode' because everything is handled by tree-sitter.
  (add-hook 'julia-ts-mode-hook (lambda () (highlight-numbers-mode -1)))

  ;; We need to increase the connection timeout of Eglot to allow enough initialization time
  ;; for the server.
  (setq-hook! 'julia-ts-mode-hook eglot-connect-timeout (max eglot-connect-timeout 120))

  ;; We must replace epsilon in Julia mode because JetBrainsMono does not have a glyph to
  ;; the lunate version of it that is used by default in Julia.
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

;; YAS snippet expand does not work well with tree-sitter without this modification.
(setq yas-indent-line 'fixed)

;; Configure eglot-jl.
(after! eglot-jl
  (setq eglot-jl-language-server-project eglot-jl-base)
  (eglot-jl-init))

(provide 'setup-julia-ts-mode)
