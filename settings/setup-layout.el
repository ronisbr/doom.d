;;; settings/setup-layout.el --- Frame layout similar to NANO Emacs -*- lexical-binding: t; -*-

(setq default-frame-alist
      (append (list
               '(min-height . 1)
               '(height     . 45)
               '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 12)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(add-hook 'term-mode-hook
          (lambda () (setq buffer-display-table (make-display-table))))

;; Vertical window divider
(setq window-divider-default-right-width 10)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(provide 'setup-layout)
