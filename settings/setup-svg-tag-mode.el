;;; settings/setup-svg-tag-mode.el --- Setup svg-tag-mode -*- lexical-binding: t; -*-

(use-package! svg-tag-mode
  :hook (org-mode . svg-tag-mode)
  :config
  (plist-put svg-lib-style-default :font-family "JetBrainsMono Nerd Font")
  (plist-put svg-lib-style-default :font-size 13)

  (setq svg-tag-tags
        `(("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0)))))))

(provide 'setup-svg-tag-mode)
