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

(defun ronisbr/org-export-process-name-tags (s backend info)
  "Function to process the name tags when exporting.

This function performs the following replacements in the string S:
- `@(NAME[ SURNAME]) => *NAME[ SURNAME]*'"
  (replace-regexp-in-string "@(\\(\\(\\w+\\|\\s-\\)+\\))" "*\\1*" s))

(after! org
  (defun ronisbr/org-insert-heading-time ()
    "Insert the property `TIME' to the current Org heading."
    (interactive)
    (let* (
           ;; Try to obtain the date from the Org title.
           (ptitle-date  (parse-time-string (org-get-title)))
           (title-day    (nth 3 ptitle-date))
           (title-month  (nth 4 ptitle-date))
           (title-year   (nth 5 ptitle-date))
           (title-date-p (and title-day title-month title-year))

           ;; If the title does not have a valid date, use the current date.
           (pnow         (decode-time))
           (propday      (if title-date-p title-day   (nth 3 pnow)))
           (propmonth    (if title-date-p title-month (nth 4 pnow)))
           (propyear     (if title-date-p title-year  (nth 5 pnow)))

           ;; Try to obtain the property hour from the heading. If it is not
           ;; possible, ask the user for a hour.
           (org-heading  (nth 4 (org-heading-components)))
           (hourstr      (progn
                           (if (string-match "^\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" org-heading)
                               (match-string 1 org-heading)
                             (read-string "Enter the hour [HH:MM]: "))))
           (prophour     (nth 2 (parse-time-string hourstr)))
           (propmin      (nth 1 (parse-time-string hourstr)))

           ;; Parse the timestamp to be added
           (timestamp    (format-time-string "<%Y-%m-%d %a %H:%M>"
                                             (encode-time `(0
                                                            ,propmin
                                                            ,prophour
                                                            ,propday
                                                            ,propmonth
                                                            ,propyear
                                                            nil
                                                            -1
                                                            nil)))))

      (save-excursion
        (org-back-to-heading)
        (org-set-property "TIME" timestamp)))))

;; =============================================================================
;;                                    Org
;; =============================================================================

(after! org
  (custom-set-faces!
    `(doom-themes-org-at-tag :foreground ,(doom-color 'nano-salient)))

  (setq org-agenda-files (list ronisbr/org-gtd-inbox-file
                               ronisbr/org-gtd-project-file
                               ronisbr/org-gtd-tickler-file
                               "~/Nextcloud/org/Roam/Diário/"))
  (setq org-edit-src-content-indentation 0)
  (setq org-image-actual-width 400)
  (setq org-log-done 'time)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-src-preserve-indentation nil)
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
  (setq org-todo-keyword-faces
        '(("WAIT" . +org-todo-onhold)
          ("STRT" . +org-todo-active)
          ("DLGT" . +org-todo-active)
          ("CANC" . +org-todo-cancel)
          ("[?]"  . +org-todo-onhold)
          ("[-]"  . +org-todo-active)
          ("[>]"  . +org-todo-active)
          ("[!]"  . +org-todo-cancel)))

  ;; Do not use highlight `@' and `#' using Doom's default pattern.
  (setq doom-themes-org-fontify-special-tags nil)

  ;; This new patters allow accents in `@' and `#' as well as spaces when using
  ;; with parenthesis.
  (font-lock-add-keywords
   'org-mode
   `((,(rx (or bol space)
           (group (group (or "#" "@"))
                  (group (or (one-or-more word)
                             (and "(" (one-or-more (or word space)) ")")))))
      1
      (doom-themes--org-tag-face 2)
      prepend))
   t))

(after! ox
  ;; Add filter to replace the tags with the names when exporting.
  (add-to-list 'org-export-filter-body-functions
               'ronisbr/org-export-process-name-tags))

(after! org-archive-subtree-hierarchically
  (setq org-archive-default-command 'org-archive-subtree-hierarchically))

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
   org-agenda-current-time-string "◀ ----- now")

  ;; Those advice were designed when using a bottom modeline. Since we are using
  ;; a header line, we must remove them.
  (advice-remove 'org-fast-tag-selection #'+popup--org-fix-popup-window-shrinking-a)
  (advice-remove 'org-fast-todo-selection #'+popup--org-fix-popup-window-shrinking-a))

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
;;                             Org Fancy Priorities
;; =============================================================================

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲")))

;; =============================================================================
;;                                 Org Roam
;; =============================================================================

(after! org-roam
  :config
  (setq org-roam-directory "~/Nextcloud/org/Roam/")
  (setq org-roam-dailies-directory "Diário/")
  (setq org-roam-capture-templates
        '(("d" "Geral"
           plain "%?"
           :target (file+head "Geral/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("n" "Notas"
           plain "%?"
           :target (file+head "Notas/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("t" "Trabalho"
           plain "%?"
           :target (file+head "Trabalho/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default"
           entry "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))))
  (setq org-roam-node-display-template
        (format "%s ${doom-hierarchy:*} %s"
                (propertize "${doom-type:10}" 'face 'font-lock-keyword-face)
                (propertize "${doom-tags:25}" 'face 'org-tag))))

(provide 'setup-org-mode)
