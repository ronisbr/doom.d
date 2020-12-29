;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ronan Arraes Jardim Chagas"
      user-mail-address "ronisbr@gmail.com")

(setq doom-font (font-spec :family "MesloLGMDZ Nerd Font" :size 13))

;; Select locale.
(setenv "LANG" "pt_BR.UTF-8")
(setenv "LC_CTYPE" "pt_BR.UTF-8")
(setenv "LC_ALL" "pt_BR.UTF-8")
(set-locale-environment "pt_BR.UTF-8")

(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/org"
      org-agenda-files '("~/Nextcloud/org/Agenda"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Custom settings.
(add-load-path! "./settings")
(require 'setup-auctex)
(require 'setup-evil)
(require 'setup-keybindings)
(require 'setup-neotree)
(require 'setup-prog-mode)
(require 'setup-org-mode)
(require 'setup-spell)

;; Local functions.
(add-load-path! "./local-lisp")
(require 'comment-align)
(require 'fill-line)
(require 'julia-local)
(require 'text-align)
