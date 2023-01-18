;;; ~/.doom.d/settings/setup-org-mode.el -*- lexical-binding: t; -*-
;;
;; Configure the org-mode.

;; =============================================================================
;;                            Org files definition
;; =============================================================================

;; Default directory of Org files.
(defvar ronisbr/org-gtd-directory
  "~/Nextcloud/org/GTD/")

(defvar ronisbr/+org-gtd-inbox-file
  "Caixa de entrada.org")

(defvar ronisbr/+org-gtd-project-file
  "Projetos.org")

(defvar ronisbr/+org-gtd-someday-file
  "Algum dia.org")

(defvar ronisbr/+org-gtd-tickler-file
  "Gaveteiro.org")

(setq ronisbr/org-gtd-inbox-file
      (expand-file-name ronisbr/+org-gtd-inbox-file ronisbr/org-gtd-directory))
(setq ronisbr/org-gtd-project-file
      (expand-file-name ronisbr/+org-gtd-project-file ronisbr/org-gtd-directory))
(setq ronisbr/org-gtd-someday-file
      (expand-file-name ronisbr/+org-gtd-someday-file ronisbr/org-gtd-directory))
(setq ronisbr/org-gtd-tickler-file
      (expand-file-name ronisbr/+org-gtd-tickler-file ronisbr/org-gtd-directory))

;; =============================================================================
;;                                    Org
;; =============================================================================

(after! org
  (setq org-agenda-files (list ronisbr/org-gtd-inbox-file
                               ronisbr/org-gtd-project-file
                               ronisbr/org-gtd-tickler-file))
  (setq org-log-done 'time)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-image-actual-width 400)
  (setq org-startup-folded 'content)
  (setq org-tags-column +100)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "WAIT(w)"
           "STRT(s!)"
           "DLGT(p!)"
           "|"
           "DONE(d!)"
           "CANC(c@)")
          (sequence
           "[ ](T)"
           "[?](W)"
           "[-](S!)"
           "[>](P!)"
           "|"
           "[✓](D!)"
           "[!](C@)")))
  (setq org-todo-keywords-faces
        '(("WAIT" . +org-todo-onhold)
          ("STRT" . +org-todo-active)
          ("DLGT" . +org-todo-active)
          ("CANC" . +org-todo-cancel)
          ("[?]"  . +org-todo-onhold)
          ("[-]"  . +org-todo-active)
          ("[>]"  . +org-todo-active)
          ("[!]"  . +org-todo-cancel))))

;; Se the default dictionary in org-mode to Brazilian Portuguese.
(add-hook! 'org-mode-hook (ispell-change-dictionary "pt_BR"))

;; =============================================================================
;;                                 Org agenda
;; =============================================================================

(after! org
  (setq-default
   org-agenda-window-setup 'current-window
   org-agenda-skip-unavailable-files t
   org-agenda-start-day "-3d"
   org-agenda-span 10
   org-agenda-repeating-timestamp-show-all nil
   org-agenda-remove-tags t
   org-agenda-prefix-format " %-11.11c %?-12t% s"
   org-agenda-todo-keyword-format "✓️"
   org-agenda-scheduled-leaders '("Scheduled  " "Sched.%2dx  ")
   org-agenda-deadline-leaders '("Deadline   " "In %3d d.  " "%2d d. ago  ")
   org-agenda-time-grid '((daily today remove-match)
                          (0800 1000 1200 1400 1600 1800 2000 2200)
                          "      " "-----------")
   org-agenda-current-time-string "◀ ----- now"))

;; =============================================================================
;;                                Org capture
;; =============================================================================

(after! org
  (setq org-capture-templates
        (doct `((,(format "%s\tTarefa" (all-the-icons-octicon "inbox" :face 'all-the-icons-green :v-adjust 0.01))
                 :keys "t"
                 :file ronisbr/org-gtd-inbox-file
                 :headline "Tarefas"
                 :prepend t
                 :type entry
                 :template ("* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:"
                            "%i"))
                (,(format "%s\tGaveteiro" (all-the-icons-octicon "briefcase" :face 'all-the-icons-yellow :v-adjust 0.01))
                 :keys "g"
                 :file ronisbr/org-gtd-tickler-file
                 :headline "Gaveteiro"
                 :prepend t
                 :type entry
                 :template ("* %?\n%^{Início:}t\n:PROPERTIES:\n:CREATED: %U\n:END:"
                            "%i"))))))

;; =============================================================================
;;                                 Org clock
;; =============================================================================

;; Automatically save the file after clock in and out.
(add-hook 'org-clock-in-hook #'save-buffer)
(add-hook 'org-clock-out-hook #'save-buffer)

;; =============================================================================
;;                                 Org Roam
;; =============================================================================

(after! org
  :config
  (setq org-roam-directory "~/Nextcloud/org/Roam/")
  (setq org-roam-dailies-directory "Diário/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default"
           entry "* %U %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")))))

(provide 'setup-org-mode)
