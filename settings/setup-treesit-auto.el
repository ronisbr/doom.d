;;; settings/setup-treesit-auto.el --- Configure treesit-auto -*- lexical-binding: t; -*-

(use-package! treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'setup-treesit-auto)
