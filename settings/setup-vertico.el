;;; settings/setup-vertico.el -*- lexical-binding: t; -*-
;;
;; Configure the vertico.


;; =============================================================================
;;                              Vertico-posframe
;; =============================================================================

(use-package! vertico-posframe
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10)))
  (setq vertico-posframe-border-width 0)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)

  (setq vertico-multiform-commands
      '((consult-line
         posframe
         (vertico-posframe-fallback-mode . vertico-buffer-mode))
        (t posframe)))

  (vertico-multiform-mode 1))

(provide 'setup-vertico)
