;;; settings/setup-appearance.el --- Configure the general appearance -*- lexical-binding: t; -*-

(use-package! nano-modeline
  :after doom-modeline
  :config
  (nano-modeline)
  (nano-modeline-mode 1))

(use-package! nano-theme)

;; Font configuration
;; =============================================================================

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))

;; For some reason, when using emacs-plus, the greek fonts changes for lines
;; that are not selected when starting Julia mode.
(add-hook! 'after-setting-font-hook
  (set-fontset-font t 'greek "JetBrainsMono Nerd Font"))

;; Theme configuration
;; =============================================================================

(setq doom-theme 'nil)
(load-theme 'nano-dark t)

;; General appearance configuration
;; =============================================================================

(setq display-line-numbers-type nil
      evil-default-cursor t
      custom-blue "#718591"
      custom-yellow "#BDA441")

; still haven't figured out what is up with point
; related to nano. this is a stopgap measure to
; get a decent looking cursor
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-cursor-color custom-blue))))
  (set-cursor-color custom-blue))

(defun set-yellow ()
  (interactive)
  (set-cursor-color custom-yellow))
(defun set-blue ()
  (interactive)
  (set-cursor-color custom-blue))

;; Enable smooth scrolling by default.
(pixel-scroll-precision-mode 1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; =============================================================================
;; Configurations for the theme monokai
;; =============================================================================

;; The theme `doom-monokai-classic' does not set the color for variables.
;; (custom-theme-set-faces! '(doom-monokai-classic)
;;   `(font-lock-variable-name-face :foreground ,(doom-lighten "#9C91E4" 0.45)))

;; Tweak indent-guides for the monokai theme. Otherwise, the guides are barely
;; visible.
;; (after! highlight-indent-guides
;;   (setq highlight-indent-guides-auto-odd-face-perc 80)
;;   (setq highlight-indent-guides-auto-even-face-perc 80)
;;   (setq highlight-indent-guides-auto-character-face-perc 90))

(provide 'setup-appearance)
