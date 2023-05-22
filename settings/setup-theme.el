;;; settings/setup-theme.el --- Configure the theme -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Noto Sans Mono SemiCondensed" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans Mono SemiCondensed" :size 14))

;; For some reason, when using emacs-plus, the greek fonts changes for lines that are not
;; selected when starting Julia mode.
(add-hook! 'after-setting-font-hook
  (set-fontset-font t 'greek "Noto Sans Mono SemiCondensed")
  ;; Some emojis we do not want to use Apple's font.
  (set-fontset-font t ?🅰 "Noto Sans Mono SemiCondensed")
  (set-fontset-font t ?🅱 "Noto Sans Mono SemiCondensed"))

(after! doom-themes
  (load-theme 'doom-nano-light t))

;; The theme `doom-monokai-classic' does not set the color for variables.
(custom-theme-set-faces! '(doom-monokai-classic)
  `(font-lock-variable-name-face :foreground ,(doom-lighten "#9C91E4" 0.45)))

;; Set the window divider color every time we change the theme.
(add-hook! 'doom-load-theme-hook
  (custom-set-faces!
    `(window-divider :foreground ,(doom-color 'bg))
    `(vertical-border :foreground ,(doom-color 'bg))))

;; Tweak indent-guides for the monokai theme. Otherwise, the guides are barely
;; visible.
;; (after! highlight-indent-guides
;;   (setq highlight-indent-guides-auto-odd-face-perc 80)
;;   (setq highlight-indent-guides-auto-even-face-perc 80)
;;   (setq highlight-indent-guides-auto-character-face-perc 90))

;; Fix the problem when the theme is not correct when in daemon mode.
;;
;; See: https://github.com/doomemacs/doomemacs/issues/6221
(defun load-doom-theme (frame)
  (select-frame frame)
  (load-theme doom-theme t))

(when (daemonp)
    (add-hook 'after-make-frame-functions #'load-doom-theme))

(provide 'setup-theme)
