;; org-archive-subtree-hierarchically.el -- Archive items in Org mode hierarchically -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file defines function to archive an org subtree hierarchically.
;;

(require 'org-archive)

;;; Code:

(defun org-archive-subtree-hierarchical--line-content-as-string ()
  "Return the content of the current line as a string."
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun org-archive-subtree-hierarchical--org-child-list ()
  "Return all children of a heading as a list."
  (interactive)
  (save-excursion
    ;; This only works with org-version > 8.0, since in previous org-mode versions the
    ;; function (org-outline-level) returns gargabe when the point is not on a heading.
    (if (= (org-outline-level) 0)
        (outline-next-visible-heading 1)
      (org-goto-first-child))
    (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
      child-list)))

(defun org-archive-subtree-hierarchical--org-struct-subtree ()
  "Return the tree structure in which a subtree belongs as a list."
  (interactive)
  (let ((archive-tree nil))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))

(defun org-archive-subtree-hierarchically ()
  "Archives a subtree hierarchically."
  (interactive)
  (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
        (this-buffer (current-buffer))
        (file (abbreviate-file-name
               (or (buffer-file-name (buffer-base-buffer))
                   (error "No file associated to buffer")))))
    (save-excursion
      (setq location org-archive-location
            afile (car (org-archive--compute-location
		                   (or (org-entry-get nil "ARCHIVE" 'inherit) location)))
            ;; heading (org-extract-archive-heading location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))
      (if (> (length afile) 0)
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                buffer (or visiting (find-file-noselect afile)))
        (setq buffer (current-buffer)))
      (unless buffer
        (error "Cannot access file \"%s\"" afile))
      (org-cut-subtree)
      (set-buffer buffer)
      (org-mode)
      (goto-char (point-min))
      (while (not (equal org-tree nil))
        (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
          (if (member (car org-tree) child-list)
              (progn
                (search-forward (car org-tree) nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (goto-char (point-max))
              (newline)
              (org-insert-struct org-tree)
              (setq org-tree nil)))))
      (newline)
      (org-yank)
      (when (not (eq this-buffer buffer))
        (save-buffer))
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))

(defun org-insert-struct (struct)
  "Insert the org STRUCT in the current point."
  (interactive)
  (when struct
    (insert (car struct))
    (newline)
    (org-insert-struct (cdr struct))))

(provide 'org-archive-subtree-hierarchically)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; org-archive-subtree-hierarchically.el ends here
