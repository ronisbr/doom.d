;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ronan Arraes Jardim Chagas"
      user-mail-address "ronisbr@gmail.com")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))

;; For some reason, when using emacs-plus, the greek fonts changes for lines
;; that are not selected when starting Julia mode.
(add-hook! 'after-setting-font-hook
  (set-fontset-font t 'greek "JetBrainsMono Nerd Font"))

;; Select locale.
(setenv "LANG" "pt_BR.UTF-8")
(setenv "LC_CTYPE" "pt_BR.UTF-8")
(setenv "LC_ALL" "pt_BR.UTF-8")
(set-locale-environment "pt_BR.UTF-8")

(setq doom-theme 'doom-monokai-classic)

;; The theme `doom-monokai-classic' does not set the color for variables.
(custom-theme-set-faces! '(doom-monokai-classic)
  `(font-lock-variable-name-face :foreground ,(doom-lighten "#9C91E4" 0.45)))

;; Tweak indent-guides for the monokai theme. Otherwise, the guides are barely
;; visible.
(after! highlight-indent-guides
  (setq highlight-indent-guides-auto-odd-face-perc 80)
  (setq highlight-indent-guides-auto-even-face-perc 80)
  (setq highlight-indent-guides-auto-character-face-perc 90))

;; Enable smooth scrolling by default.
(pixel-scroll-precision-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/org"
      org-agenda-files '("~/Nextcloud/org/Agenda"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Custom settings.
(add-load-path! "~/.doom.d/settings")
(require 'setup-auctex)
(require 'setup-company)
(require 'setup-emojify)
(require 'setup-evil)
(require 'setup-keybindings)
(require 'setup-julia-ts-mode)
(require 'setup-menu-bar)
(require 'setup-org-mode)
(require 'setup-prog-mode)
(require 'setup-spell)
(require 'setup-treemacs)
(require 'setup-vertico)
(require 'setup-vterm)

;; Local functions.
(add-load-path! "~/.doom.d/local-lisp")
(require 'comment-align)
(require 'fill-line)
(require 'julia-local)
(require 'text-align)
