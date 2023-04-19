;;; ~/.doom.d/settings/setup-julia-ts-mode.el --- Configure julia-ts-mode -*- lexical-binding: t; -*-

(defun ronisbr/flyspell-julia-ts-mode-verify ()
  "Verify if a word predicate must have its spell verified in `julia-ts-mode'.

The default algorithm does not work properly using strings because tree-sitter
decorates a string using a list of faces. This behavior is required to
highlight, for example, string interpolations. Hence, in a string, the text
property is `(font-string-lock-face)' that does not match
`font-string-lock-face' in the default verification algorithm."
  (unless (eql (point) (point-min))
    ;; (point) is next char after the word. Must check one char before.
    (let ((f (ensure-list (get-text-property (1- (point)) 'face))))
      ;; We must only check the spell if the text has only one decoration,
      ;; avoiding verifying string interpolations, for example.
      (when (eq (length f) 1)
        (memq (car f) flyspell-prog-text-faces)))))

(use-package! julia-ts-mode
  :mode "\\.jl$")

(after! julia-ts-mode
  ;; We do not need `highlight-numbers-mode' because everything is handled by tree-sitter.
  (add-hook 'julia-ts-mode-hook (lambda () (highlight-numbers-mode -1)))

  ;; We need to increase the connection timeout of Eglot to allow enough initialization time
  ;; for the server.
  (setq-hook! 'julia-ts-mode-hook eglot-connect-timeout (max eglot-connect-timeout 120))

  ;; Set the word predicate check function for flyspell.
  (setq-hook! 'julia-ts-mode-hook
    flyspell-generic-check-word-predicate #'ronisbr/flyspell-julia-ts-mode-verify)

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
