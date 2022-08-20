;;; settings/setup-vterm.el -*- lexical-binding: t; -*-
;;
;; Configuration for vterm.

(defun vterm--send-C-d ()
  "Send <C-d> to vterm."
  (interactive)
  (when vterm--term
    (vterm-send-key "d" nil nil 0)))

;; Map [kp-delete] to send <C-d>. Otherwise, the delete key does not work in
;; GUI.
(map! :after vterm
      :map vterm-mode-map
      [kp-delete] #'vterm--send-C-d)

;; Map [kp-delete] to send <C-d>. Otherwise, the delete key does not work in
;; terminal.
(map! :after vterm
      :map vterm-mode-map
      [deletechar] #'vterm--send-C-d)

(provide 'setup-vterm)
