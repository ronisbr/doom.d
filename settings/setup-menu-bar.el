;;; ~/.doom.d/settings/setup-menu-bar.el -*- lexical-binding: t; -*-
;;
;; Configure Emacs menu bar.

(set-face-attribute 'menu nil
                    :inverse-video nil
                    :background "#1C1E23"
                    :bold nil
                    :foreground "#D7D5D1")

(set-face-attribute 'tty-menu-enabled-face nil
                    :inverse-video nil
                    :background "#1C1E23"
                    :bold nil
                    :foreground "#D7D5D1")

(set-face-attribute 'tty-menu-disabled-face nil
                    :inverse-video nil
                    :background "#1C1E23"
                    :bold nil
                    :foreground "#5A6065")

(set-face-attribute 'tty-menu-selected-face nil
                    :inverse-video nil
                    :background "#292C33"
                    :bold nil
                    :foreground "#D7D5D1")

(menu-bar-mode t)

(provide 'setup-menu-bar)
