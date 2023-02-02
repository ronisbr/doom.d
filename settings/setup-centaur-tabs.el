;;; ~/.doom.d/settings/setup-centaur-tabs.el -*- lexical-binding: t; -*-
;;
;; Configure centaur-tabs.

(after! centaur-tabs
  (setq centaur-tabs-height 25
        centaur-tabs-modified-marker "●"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-close-button nil
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-style "alternate"
        x-underline-at-descent-line t)
  (map! "C-S-<right>" #'centaur-tabs-forward)
  (map! "C-S-<left>" #'centaur-tabs-backward))

;; Fix centaur-tabs when using emacsclient.
;;    See: https://github.com/ema2159/centaur-tabs/issues/127
;;         https://github.com/doomemacs/doomemacs/issues/6647

(after! centaur-tabs
  (setq centaur-tabs-set-bar 'right))

(defun ronisbr/fix-centaur-tabs ()
  (centaur-tabs-mode -1)
  (centaur-tabs-mode)
  (centaur-tabs-headline-match)
  ;; This function is executed when a new frame is created. In this case, most
  ;; of time we will be in dashboard. This function ensures that the
  ;; centaur-tabs are now shown in the dashboard.
  (centaur-tabs-local-mode))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (ronisbr/fix-centaur-tabs)))))

(provide 'setup-centaur-tabs)
