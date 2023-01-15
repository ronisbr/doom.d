;;; settings/setup-vertico.el -*- lexical-binding: t; -*-
;;
;; Configure the vertico.


;; =============================================================================
;;                              Vertico-posframe
;; =============================================================================

(after! vertico-posframe
  (setq vertico-posframe-border-width 0)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center))

(provide 'setup-vertico)
