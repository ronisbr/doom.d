;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       :completion
       company
       vertico

       :ui
       doom
       doom-dashboard
       (emoji +github)
       hl-todo
       indent-guides
       ligatures
       modeline
       ophints
       (popup +defaults)
       treemacs
       (vc-gutter +pretty)
       vi-tilde-fringe
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       multiple-cursors
       snippets
       word-wrap

       :emacs
       dired
       electric
       undo
       vc

       :term
       vterm

       :checkers
       (spell +everywhere +flyspell +hunspell)
       grammar

       :tools
       (eval +overlay)
       lookup
       lsp
       magit
       pdf

       :os
       (:if IS-MAC macos)
       tty

       :lang
       (cc +lsp)
       emacs-lisp
       (julia +lsp)
       latex
       markdown
       (org +pretty)
       python
       qt
       sh

       :email

       :app

       :config
       ;;literate
       (default +bindings +smartparens))

;; Make evil respect visual lines.
(setq evil-respect-visual-line-mode t)

;; Disable deferred compilation.
;;
;;    See: https://github.com/doomemacs/doomemacs/issues/6811
;;
(setq native-comp-deferred-compilation nil)
(after! (doom-packages straight)
  (setq straight--native-comp-available t))
