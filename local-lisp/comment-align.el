;; =============================================================================
;;
;; comment-align.el
;;
;;      Functions to align comments.
;;
;; =============================================================================

(defun ronisbr/comment-center-align (fill field-size &optional begin end)
  "Align the comments in the region between `begin` and `end` to the center.
The align character is defined by `fill` and the considered field
size for the alignment is `field-size`. If `begin` or `end` is
`nil`, then only the current line will be aligned."
  (interactive
   (let ((fill (read-char "Fill character: "))
         (field-size (read-number "Field size: " fill-column)))
     (if (not field-size)
         (setq field-size fill-column))
     (if (use-region-p)
         (list fill field-size (region-beginning) (region-end))
       (list fill field-size))))

  (save-excursion
    ;; If `begin` was not specified, check if we are in a region.
    (if (not begin)
        (if (use-region-p)
            ;; In this case, set `begin` to the beginning of the region.
            (setq begin (region-beginning))
          ;; Otherwise, set `begin` to the beginning of the current line.
        (setq begin (line-beginning-position))))

    ;; If `end` was not specified, check if we are in a region.
    (if (not end)
        (if (use-region-p)
            ;; In this case, set `end` to the end of the region.
            (setq end (region-end))
          ;; Otherwise, set `end` to the end of the current line.
        (setq end (line-end-position))))

    ;; Count the number of lines between `begin` and `end`. The algorithm will
    ;; be applied to each line.
    (let ((numlines (count-lines begin end)))

      ;; If `begin` and `end` are equal, then `numlines` will be 0. In this case
      ;; we must execute the algorithm one time.
      (if (= numlines 0)
          (setq numlines 1))

      ;; Move to beginning of the region.
      (goto-char begin)

      (dotimes (l numlines)

        (beginning-of-line)

        ;; Get the position of the beginning of the line and the beginning of
        ;; the comment, if exits.
        (let* ((cbeg (comment-search-forward (line-end-position) t)))
          ;; If there is not comment, then just exit.
          (if cbeg
              ;; Because of `comment-search-forward`, we are currently at the
              ;; first character of the comment.
              (let ((ws (- (skip-syntax-backward "\s" (line-beginning-position)))))

                ;; - `delta` is the number of characters that must be added to
                ;;   align the comment to the right. This does not consider the
                ;;   white spaces. They will be divided by 2 to align the
                ;;   comment in the center.
                (let* ((delta (+ fill-column
                                 (- (line-beginning-position) (line-end-position))
                                 ws
                                 -3)))

                  ;; If `delta` is negative, then the size of the comment goes
                  ;; beyond the fill column. In this case, nothing will be done.
                  (cond ((= delta 0)
                         (insert-char ? ))
                        ((= delta 1)
                         (insert-char ? )
                         (insert-char fill))
                        ((> delta 1)
                         (delete-char ws)
                         (insert-char ? )
                         (insert-char fill (/ delta 2))
                         (insert-char ? )
                         (end-of-line)
                         (insert-char ? )
                         (insert-char fill (- fill-column
                                              (- (point) (line-beginning-position))))
                          ))))))
        (next-line)))))

(defun ronisbr/comment-right-align (fill field-size &optional begin end)
  "Align the comments in the region between `begin` and `end` to the right.
The align character is defined by `fill` and the considered field
size for the alignment is `field-size`. If `begin` or `end` is
`nil`, then only the current line will be aligned."
  (interactive
   (let ((fill (read-char "Fill character: "))
         (field-size (read-number "Field size: " fill-column)))
     (if (not field-size)
         (setq field-size fill-column))
     (if (use-region-p)
         (list fill field-size (region-beginning) (region-end))
       (list fill field-size))))

  (save-excursion
    ;; If `begin` was not specified, check if we are in a region.
    (if (not begin)
        (if (use-region-p)
            ;; In this case, set `begin` to the beginning of the region.
            (setq begin (region-beginning))
          ;; Otherwise, set `begin` to the beginning of the current line.
        (setq begin (line-beginning-position))))

    ;; If `end` was not specified, check if we are in a region.
    (if (not end)
        (if (use-region-p)
            ;; In this case, set `end` to the end of the region.
            (setq end (region-end))
          ;; Otherwise, set `end` to the end of the current line.
        (setq end (line-end-position))))

    ;; Count the number of lines between `begin` and `end`. The algorithm will
    ;; be applied to each line.
    (let ((numlines (count-lines begin end)))

      ;; If `begin` and `end` are equal, then `numlines` will be 0. In this case
      ;; we must execute the algorithm one time.
      (if (= numlines 0)
          (setq numlines 1))

      ;; Move to beginning of the region.
      (goto-char begin)

      (dotimes (l numlines)

        (beginning-of-line)

        ;; Get the position of the beginning of the line and the beginning of
        ;; the comment, if exits.
        (let* ((cbeg (comment-search-forward (line-end-position) t)))
          ;; If there is not comment, then just exit.
          (if cbeg
              ;; Because of `comment-search-forward`, we are currently at the
              ;; first character of the comment.
              (let ((ws (- (skip-syntax-backward "\s" (line-beginning-position)))))

                ;; - `delta` is the number of characters that must be added to
                ;;   align the comment to the right. This does not consider the
                ;;   white spaces.
                (let* ((delta (+ fill-column
                                 (- (line-beginning-position) (line-end-position))
                                 ws
                                 -2)))

                  ;; If `delta` is negative, then the size of the comment goes
                  ;; beyond the fill column. In this case, nothing will be done.
                  (cond ((= delta 0)
                         (insert-char ? ))
                        ((> delta 0)
                         (delete-char ws)
                         (insert-char ? )
                         (insert-char fill delta)
                         (insert-char ? )))))))
        (next-line)))))

(provide 'comment-align)
