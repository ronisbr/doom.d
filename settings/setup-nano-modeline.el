;;; settings/setup-nano-modeline.el -*- lexical-binding: t; -*-

(use-package! nano-modeline
  :config
  ;; If we do not call this global mode, Emacs shows doom-modeline when started.
  (global-hide-mode-line-mode)

  (nano-modeline)
  (nano-modeline-mode 1))

(provide 'setup-nano-modeline)
