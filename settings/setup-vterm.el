;;; settings/setup-vterm.el --- Configure vterm -*- lexical-binding: t; -*-

;; Improve speed when using in terminal.
;;     Ref: https://www.reddit.com/r/emacs/comments/pjtm91/vterm_a_little_bit_slow/
(after! vterm
  (setq vterm-timer-delay 0.01))

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
