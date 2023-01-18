;;; ~/.doom.d/settings/setup-evil.el -*- lexical-binding: t; -*-
;;
;; General configurations for Evil mode.

;; The scope of evil-snipe should be the entire visible buffer.
(setq evil-snipe-scope 'whole-visible)

;; =============================================================================
;;                               Evil operators
;; =============================================================================

(after! evil
  (evil-define-operator ronisbr/evil-join (beg end)
    "Join the selected lines without moving the cursor.

This advise was obtained from Doom Emacs but adapter to avoid moving the
cursor."
    :motion evil-line
    :move-point nil
    (save-excursion
      (let* ((count (count-lines beg end))
             (count (if (> count 1) (1- count) count))
             (fixup-mark (make-marker)))
        (dotimes (var count)
          (if (and (bolp) (eolp))
              (join-line 1)
            (let* ((end (line-beginning-position 3))
                   (fill-column (1+ (- end beg))))
              (set-marker fixup-mark (line-end-position))
              (fill-region-as-paragraph beg end nil t)
              (goto-char fixup-mark)
              (fixup-whitespace))))
        (set-marker fixup-mark nil))))

  (advice-add #'evil-join :override #'ronisbr/evil-join))

(provide 'setup-evil)
