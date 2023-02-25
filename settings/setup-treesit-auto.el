;;; settings/setup-treesit-auto.el -*- lexical-binding: t; -*-
;;
;; Configure treesit-auto.

(use-package! treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'setup-treesit-auto)
