;; =============================================================================
;;
;; fill-line.el
;;
;;      Functions to fill a line with a character.
;;
;; =============================================================================

(defun ronisbr/copy-to-end ()
  "Copy the current character to the rest of line up to `fill-column`."
  (interactive)
  (let ((char (char-after)))
    (save-excursion
      (kill-line)
      (ronisbr/fill-to-end char))))

(defun ronisbr/fill-to-end (char)
  "Fill the end of current line up to `fill-column` with a character."
  (interactive "cFilling character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) fill-column)
      (insert-char char))))

(defun ronisbr/fill-line (char)
  "Insert a new line and fill it up to the `fill-column` with a character."
  (interactive "cFilling character:")
  (save-excursion
    (previous-line)
    (end-of-line)
    (newline)
    (end-of-line)
    (while (< (current-column) fill-column)
      (insert-char char))))

(provide 'fill-line)
