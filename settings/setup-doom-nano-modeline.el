;;; settings/setup-doom-nano-modeline.el --- Configure doom-nano-modeline -*- lexical-binding: t; -*-

(use-package! doom-nano-modeline
  :config
  (doom-nano-modeline-mode 1)
  (global-hide-mode-line-mode 1))

(provide 'setup-doom-nano-modeline)
