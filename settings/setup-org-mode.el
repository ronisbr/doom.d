;;; ~/.doom.d/settings/setup-org-mode.el -*- lexical-binding: t; -*-
;;
;; Configure the org-mode.

;; =============================================================================
;;                            Org files definition
;; =============================================================================

;; Default directory of Org files.
(defvar ronisbr/org-directory
  "~/Nextcloud/org/")

(defvar ronisbr/org-agenda-directory
  "~/Nextcloud/org/Agenda/")

;; Journal directory.
(defvar ronisbr/+org-journal-directory
  "Diário")

;; TODO file for org-capture.
(defvar ronisbr/+org-capture-inbox-file
  "Caixa de entrada.org")

;; Wiki directory.
(defvar ronisbr/+org-wiki-directory
  "Wiki")

(setq ronisbr/org-capture-inbox-file
      (expand-file-name ronisbr/+org-capture-inbox-file ronisbr/org-agenda-directory))
(setq ronisbr/org-journal-directory
      (expand-file-name ronisbr/+org-journal-directory ronisbr/org-directory))
(setq ronisbr/org-wiki-directory
      (expand-file-name ronisbr/+org-wiki-directory ronisbr/org-directory))

;; =============================================================================
;;                                    Org
;; =============================================================================

(after! org
  (setq org-log-done 'time)
  (setq org-tags-column +81)
  (setq org-startup-folded 'content))

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
        (doct `((,(format "%s\tNota" (all-the-icons-faicon "sticky-note" :face 'all-the-icons-green :v-adjust 0.01))
                 :keys "n"
                 :file ronisbr/org-capture-inbox-file
                 :headline "Notas"
                 :hook (lambda () (ispell-change-dictionary "pt_BR"))
                 :prepend t
                 :type entry
                 :template ("* %?"
                            "%i %a"))
                (,(format "%s\tAgendamento" (all-the-icons-octicon "calendar" :face 'all-the-icons-yellow :v-adjust 0.01))
                 :keys "s"
                 :file ronisbr/org-capture-inbox-file
                 :headline "Agendamentos"
                 :hook (lambda () (ispell-change-dictionary "pt_BR"))
                 :prepend t
                 :type entry
                 :template ("* %?\nSCHEDULED: %^{Início:}t"
                            "%i"))
                (,(format "%s\tAtividade" (all-the-icons-octicon "inbox" :face 'all-the-icons-blue :v-adjust 0.01))
                 :keys "a"
                 :file ronisbr/org-capture-inbox-file
                 :headline "Atividades"
                 :hook (lambda () (ispell-change-dictionary "pt_BR"))
                 :prepend t
                 :type entry
                 :template ("* TODO %?%{extra}"
                            "%i")
                 :children ((,(format "%s\tSem prazo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                             :keys "g"
                             :extra "")
                            (,(format "%s\tCom prazo" (all-the-icons-material "timer" :face 'all-the-icons-yellow :v-adjust -0.1))
                             :keys "p"
                             :extra "\nDEADLINE: %^{Prazo:}t")
                            (,(format "%s\tCom agendamento" (all-the-icons-octicon "calendar" :face 'all-the-icons-blue :v-adjust 0.01))
                             :keys "a"
                             :extra "\nSCHEDULED: %^{Início:}t"
                             )))))))

;; =============================================================================
;;                                 Org journal
;; =============================================================================

(use-package! org-journal
  :after org
  :config
  ;; Journal directory.
  (setq org-journal-dir ronisbr/org-journal-directory)

  ;; Record journals monthly.
  (setq org-journal-file-type 'monthly)

  ;; Journal file format.
  (setq org-journal-file-format "%Y/Diário-%m.org")

  ;; Week starts on Sunday.
  (setq org-journal-start-on-weekday 0)

  ;; Set date format.
  (setq org-journal-date-format "%A, %e de %B de %Y (%d/%m/%Y)")

  ;; Header of the journal file.
  (setq org-journal-file-header
        "#+TITLE: Diário relativo ao mês %B de %Y.\n\
#+AUTHOR: Ronan Arraes Jardim Chagas\n\
#+STARTUP: content\n")

  ;; Hook after an entry is created.
  (add-hook 'org-journal-after-entry-create-hook
            (lambda ()
              ;; Always start in insert mode.
              (evil-insert-state)
              ;; The spelling language should be pt_BR.
              (ispell-change-dictionary "pt_BR"))))

(provide 'setup-org-mode)
