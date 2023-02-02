;;; settings/setup-treesit-auto.el -*- lexical-binding: t; -*-
;;
;; Configure treesit-auto.

(use-package! treesit-auto
  :config
  (add-to-list 'treesit-auto--language-source-alist
               '(julia "https://github.com/tree-sitter/tree-sitter-julia"))
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'setup-treesit-auto)
