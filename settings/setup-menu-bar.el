;;; ~/.doom.d/settings/setup-menu-bar.el --- Configure Emacs menu bar -*- lexical-binding: t; -*-

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

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (menu-bar-mode -1)))))

(provide 'setup-menu-bar)
