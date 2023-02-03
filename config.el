;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ronan Arraes Jardim Chagas"
      user-mail-address "ronisbr@gmail.com")

(add-load-path! "~/.doom.d/settings")

;; Setup the theme.
(require 'setup-theme)

;; Select locale.
(setenv "LANG" "pt_BR.UTF-8")
(setenv "LC_CTYPE" "pt_BR.UTF-8")
(setenv "LC_ALL" "pt_BR.UTF-8")
(set-locale-environment "pt_BR.UTF-8")

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
(require 'setup-auctex)
(require 'setup-centaur-tabs)
(require 'setup-company)
(require 'setup-emojify)
(require 'setup-evil)
(require 'setup-julia-ts-mode)
(require 'setup-keybindings)
(require 'setup-layout)
(require 'setup-menu-bar)
(require 'setup-nano-modeline)
(require 'setup-org-mode)
(require 'setup-prog-mode)
(require 'setup-spell)
(require 'setup-treemacs)
(require 'setup-treesit-auto)
(require 'setup-vertico)
(require 'setup-vterm)

;; Local packages.
(use-package! comment-align
  :load-path "~/.doom.d/local-lisp")
(use-package! fill-line
  :load-path "~/.doom.d/local-lisp")
(use-package! julia-local
  :load-path "~/.doom.d/local-lisp")
(use-package! text-align
  :load-path "~/.doom.d/local-lisp")

;; =============================================================================
;; Local packages
;; =============================================================================

(use-package! ox-pandoc-html
  :after org
  :load-path "~/.doom.d/local-packages/ox-pandoc-html")

(use-package! write-mode
  :load-path "~/.doom.d/local-packages/write-mode")
