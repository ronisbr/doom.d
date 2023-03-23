;;; settings/setup-svg-tag-mode.el --- Setup svg-tag-mode -*- lexical-binding: t; -*-

;; Definition of regexps to apply the SVG tags.
(defconst date-regexp             "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst day-regexp              "[A-Za-z]\\{3\\}")
(defconst time-regexp             "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-time-regexp         (format "\\(%s\\)? ?\\(%s\\)?" day-regexp time-regexp))
(defconst name-regexp             "\\(?:[[:word:]]\\|_\\)+")
(defconst name-with-spaces-regexp "\\(?:[[:word:]]\\|[[:blank:]]\\)+")

(defun ronisbr/set-svg-tag-tags-for-org-mode ()
  "Set the SVG tags for `org-mode'."
  (when (display-graphic-p)
    (setq-local svg-tag-tags

                ;; === TODO keywords ===========================================

                `(("TODO" . ((lambda (tag)
                               (svg-tag-make "TODO"
                                             :face 'org-todo
                                             :inverse t
                                             :margin 0))))

                  ("WAIT" . ((lambda (tag)
                               (svg-tag-make "WAIT"
                                             :face '+org-todo-onhold
                                             :inverse t
                                             :margin 0))))

                  ("STRT" . ((lambda (tag)
                               (svg-tag-make "STRT"
                                             :face '+org-todo-active
                                             :margin 0))))

                  ("DLGT" . ((lambda (tag)
                               (svg-tag-make "DLGT"
                                             :face '+org-todo-active
                                             :inverse t
                                             :margin 0))))

                  ("CANC" . ((lambda (tag)
                               (svg-tag-make "CANC"
                                             :face 'org-done
                                             :margin 0))))

                  ("DONE" . ((lambda (tag)
                               (svg-tag-make "DONE"
                                             :face 'org-done
                                             :margin 0))))

                  ;; === Name reference ========================================

                  (,(format "\\(@%s\\)" name-regexp) .
                   ((lambda (tag) (svg-lib-tag (substring (replace-regexp-in-string "_" " " tag) 1 nil)
                                          nil
                                          :font-weight 'bold
                                          :foreground ,(doom-color 'nano-salient)
                                          :margin 0
                                          :stroke 2))))

                  (,(format "\\(@(%s)\\)" name-with-spaces-regexp) .
                   ((lambda (tag) (svg-lib-tag (substring (replace-regexp-in-string "_" " " tag) 2 -1)
                                          nil
                                          :font-weight 'bold
                                          :foreground ,(doom-color 'nano-salient)
                                          :margin 0
                                          :stroke 2))))

                  ;; === Timestamps ============================================

                  (,(format "%s" time-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :margin 0))))

                  (,(format "\\(%s \\)-- %s" time-regexp time-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :crop-right t
                                           :margin 0))))

                  (,(format "%s \\(-- %s\\)" time-regexp time-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :beg 3
                                           :crop-left t
                                           :inverse t
                                           :margin 0))))

                  (,(format "\\(<%s>\\)" date-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :beg 1
                                           :end -1
                                           :margin 0))))

                  (,(format "\\(<%s \\)%s>" date-regexp day-time-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :beg 1
                                           :inverse nil
                                           :crop-right t
                                           :margin 0))))

                  (,(format "<%s \\(%s>\\)" date-regexp day-time-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :end -1
                                           :inverse t
                                           :crop-left t
                                           :margin 0))))

                  (,(format "\\(\\[%s\\]\\)" date-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :beg 1
                                           :end -1
                                           :margin 0
                                           :face 'org-date))))

                  (,(format "\\(\\[%s \\)%s\\]" date-regexp day-time-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :beg 1
                                           :inverse nil
                                           :crop-right t
                                           :margin 0
                                           :face 'org-date))))

                  (,(format "\\[%s \\(%s\\]\\)" date-regexp day-time-regexp) .
                   ((lambda (tag) (svg-tag-make tag
                                           :end -1
                                           :inverse t
                                           :crop-left t
                                           :margin 0
                                           :face 'org-date))))))

    (svg-tag-mode 1)))

(defun ronisbr/set-svg-tag-tags-for-prog-mode ()
  "Set the SVG tags for `prog-mode'."
  (when (display-graphic-p)
    (setq-local svg-tag-tags
                `(("TODO:" . ((lambda (tag)
                                (svg-tag-make "TODO"
                                              :face '(:foreground ,(doom-color 'nano-popout))
                                              :inverse t
                                              :margin 0))))
                  ("NOTE:" . ((lambda (tag)
                                (svg-tag-make "NOTE"
                                              :face '(:foreground ,(doom-color 'nano-salient))
                                              :inverse t
                                              :margin 0))))))

    (svg-tag-mode 1)))

(use-package! svg-tag-mode
  :init
  (add-hook 'org-mode-hook  #'ronisbr/set-svg-tag-tags-for-org-mode)
  (add-hook 'prog-mode-hook #'ronisbr/set-svg-tag-tags-for-prog-mode)

  :config
  (plist-put svg-lib-style-default :font-family "JetBrainsMono Nerd Font")
  (plist-put svg-lib-style-default :font-size 13))

(provide 'setup-svg-tag-mode)
