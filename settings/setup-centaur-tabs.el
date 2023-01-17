;;; ~/.doom.d/settings/setup-centaur-tabs.el -*- lexical-binding: t; -*-
;;
;; Configure centaur-tabs.

(after! centaur-tabs
  (map! "C-S-<right>" #'centaur-tabs-forward)
  (map! "C-S-<left>" #'centaur-tabs-backward))

(provide 'setup-centaur-tabs)
