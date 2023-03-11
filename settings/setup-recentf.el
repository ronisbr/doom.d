;;; settings/setup-recentf.el --- Cofigure recentf -*- lexical-binding: t; -*-

;; Save the recent file list every 5 min.
(after! recentf
  ;; If we do not load the recent file list first, it saves an empty file.
  (recentf-load-list)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(provide 'setup-recentf)
