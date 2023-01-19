;;; ~/.doom.d/settings/setup-centaur-tabs.el -*- lexical-binding: t; -*-
;;
;; Configure centaur-tabs.

(after! centaur-tabs
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
  (centaur-tabs-headline-match))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (ronisbr/fix-centaur-tabs)))
              (ronisbr/fix-centaur-tabs)))

(provide 'setup-centaur-tabs)
