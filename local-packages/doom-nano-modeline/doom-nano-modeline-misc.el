;;; doom-nano-modeline-misc.el -- Miscellaneous functions for doom-nano-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ronan Arraes Jardim Chagas
;;
;; This package was highly based on N Λ N O modeline by Nicolas P. Rougier
;; <Nicolas.Rougier@inria.fr>.
;;
;; Author           : Ronan Arraes Jardim Chagas
;; Created          : January 2023
;; Keywords         : mode-line, header-line
;; Package-Requires : ((emacs "28") (doom-themes "2"))
;; URL              : https://github.com/ronisbr/doom-nano-modeline
;; Version          : 0.1.0
;;
;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;; Miscellaneous functions for doom-nano-modeline.

;;; Code

(defun doom-nano-modeline-status ()
  "Return buffer status, one of 'read-only, 'modified or 'read-write."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((read-only   buffer-read-only)
         (modified    (and buffer-file-name (buffer-modified-p))))
      (cond (modified  'modified)
            (read-only 'read-only)
            (t         'read-write)))))

(defun doom-nano-modeline-buffer-name-vc-and-major-mode ()
  "Return the buffer name and the major mode."
  (let* ((buffer-name (cond
                       ((and (derived-mode-p 'org-mode)
                             (buffer-narrowed-p)
                             (buffer-base-buffer))
                        (format"%s [%s]" (buffer-base-buffer)
                               (org-link-display-format
                                (substring-no-properties (or (org-get-heading 'no-tags)
                                                             "-")))))
                       ((and (buffer-narrowed-p)
                             (buffer-base-buffer))
                        (format"%s [narrow]" (buffer-base-buffer)))
                       (t
                        (format-mode-line "%b"))))

         (buffer-modified (if (and buffer-file-name (buffer-modified-p)) "** " ""))

         (mode-name (format-mode-line mode-name))

         (vc-branch-name (doom-nano-modeline--get-vc-branch))

         (vc-branch (if vc-branch-name
                        `((vc-branch-name . nil))
                      nil)))

    `((,(concat buffer-modified buffer-name) . nil)
      (" " . nil)
      (,(if vc-branch-name (concat "[" vc-branch-name "]") "") . doom-nano-modeline-vc-branch-name-face)
      (,(if vc-branch-name " " "") . nil)
      (,(concat "(" mode-name ")") . doom-nano-modeline-major-mode-face))))

(defun doom-nano-modeline-org-clock-timer ()
  "Return the string with the current task time or nil if there is not an active clock."
  (when (and (boundp 'org-mode-line-string) (> (length org-mode-line-string) 0))
    (let* ((str (substring-no-properties org-mode-line-string))
           (matches (string-match "\\(\\[[^\\]*\\]\\)" str))
           (time-string (match-string 1 str)))
      (if time-string
          `((,time-string . doom-nano-modeline-org-clock-face))
        nil))))

(defun doom-nano-modeline-org-mode-buffer-name-and-major-mode ()
  "Return the buffer name and the major mode for Org buffers."
  (if (derived-mode-p 'org-mode)
      (let* ((org-title (doom-nano-modeline--get-org-title))
              (buffer-name (if org-title
                               org-title
                             (format-mode-line "%b")))
              (mode-name (format-mode-line mode-name))
              (buffer-modified (if (and buffer-file-name (buffer-modified-p)) "** " "")))

         `((,(concat buffer-modified buffer-name) . nil)
           (" " . nil)
           (,(concat "(" mode-name ")") . doom-nano-modeline-major-mode-face)))
    (doom-nano-modeline-default-mode)))

(defun doom-nano-modeline-cursor-position ()
  "Return the cursor position in the current buffer."
  `((,(format-mode-line "%l:%c") . nil)))

(defun doom-nano-modeline--get-org-title ()
  "Get the `+title' property of an org file. If it does not exits, return nil."
  (let ((org-title (org-collect-keywords '("TITLE"))))
    (if org-title
        (car (cdr (car org-title)))
      nil)))

(defun doom-nano-modeline--get-vc-branch ()
  "Return current VC branch if any."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun doom-nano-modeline--space ()
  "Function to return a space for the modeline render function."
  `((" " . nil)))

(defun doom-nano-modeline--vterm-set-title-advice (title)
  "Advice to `vterm--set-title' to track the current directory in `default-dir'."
  (let ((dir (string-trim-left (concat (nth 1 (split-string title ":")) "/"))))
    (when (file-directory-p dir)
      (cd-absolute dir))))

(provide 'doom-nano-modeline-misc)
