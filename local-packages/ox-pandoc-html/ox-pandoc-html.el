;;; ox-pandoc-html.el --- An Org-mode exporter to HTML using pandoc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ronan Arraes Jardim Chagas
;;
;; Author           : Ronan Arraes Jardim Chagas
;; Created          : January 2023
;; Keywords         : org pandoc
;; Package-Requires : ((emacs "29") (org "9"))
;; URL              : https://github.com/ronisbr/doom.d
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
;; This package contains functions to export Org files to HTML using Pandoc.

(require 'ox-org)

;;;; Code:

(defgroup org-pandoc-html nil
  "Options related with the Org export backend to HTML using Pandoc."
  :tag "Org Pandoc HTML"
  :prefix "org-pandoc-html-"
  :group 'org-export)

(defvar org-pandoc-html-export-to-html5-additional-options
  ""
  "Additional options passed to Pandoc when exporting to HTML5.")

;; Register the `pandoc-html' backend as a derived backend of `org'.
(org-export-define-derived-backend 'pandoc-html 'org
  :menu-entry `(?p "Export via Pandoc"
                   ((?h "To HTML5 file" org-pandoc-html-export-to-html5-file)
                    (?H "As HTML5 buffer" org-pandoc-html-export-as-html5-buffer)
                    (?v "As HTML5 + open (webkit)" org-pandoc-html-export-to-webkit))))

(defun org-pandoc-html-export-to-html5-file (&optional async subtreep visible-only body-only ext-plist)
  "Export the current Org buffer to a HTML5 file.

The options ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST has the same
meaning as in `org-export-to-file'."
  (interactive)

  ;; We will export the new org file to `file' and then convert it to HTML5
  ;; using Pandoc.
  (let* ((file (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'pandoc-html file
      async
      subtreep
      visible-only
      body-only
      ext-plist
      #'org-pandoc-html--convert-org-file-to-html5)))

(defun org-pandoc-html-export-as-html5-buffer (&optional async subtreep visible-only body-only ext-plist)
  "Export the current Org buffer to a HTML5 buffer.

The options ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST has the same
meaning as in `org-export-to-file'."
  (interactive)

  ;; We will export the Org file to a buffer and convert it to HTML5 using
  ;; Pandoc.
  (org-export-to-buffer 'pandoc-html (get-buffer-create "Org Pandoc Export HTML 5")
    async
    subtreep
    visible-only
    body-only
    ext-plist
    #'org-pandoc-html--convert-org-buffer-to-html5))

(defun org-pandoc-html-export-to-webkit (&optional async subtreep visible-only body-only ext-plist)
  "Export the current Org buffer to a temporary HTML5 file and open in Webkit.

The options SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST has the same
meaning as in `org-export-to-file'. However, ASYNC is neglected here because we
cannot have an async process that opens a webkit buffer in the current process."
  (interactive)

  ;; We will export the new org file to `file' and then convert it to HTML5
  ;; using Pandoc.
  (let* ((tmpfile (make-temp-file "org-mode-html" nil ".html")))
    (org-export-to-file 'pandoc-html tmpfile
      nil ;; We cannot use async since we need to open a Webkit buffer.
      subtreep
      visible-only
      body-only
      ext-plist
      #'org-pandoc-html--convert-org-file-to-html5-and-open-in-webkit)))

(defun org-pandoc-html--convert-org-file-to-html5 (file)
  "Convert the Org FILE to HTML 5, overwriting FILE."
  (let ((inhibit-message t))
    (shell-command (format "pandoc -f org -t html5 --mathjax --standalone %s %s -o %s"
                           org-pandoc-html-export-to-html5-additional-options
                           file
                           file)))
  file)

(defun org-pandoc-html--convert-org-file-to-html5-and-open-in-webkit (file)
  "Convert the Org FILE to HTML 5, overwriting FILE, and open it in Webkit."
  (org-pandoc-html--convert-org-file-to-html5 file)
  (xwidget-webkit-browse-url (concat "file://" file))
  file)

(defun org-pandoc-html--convert-org-buffer-to-html5 ()
  "Convert the current Org buffer to HTML 5, overwriting its contents."
  (shell-command-on-region (point-min)
                           (point-max)
                           (concat "pandoc -f org -t html5 --mathjax --standalone"
                                   org-pandoc-html-export-to-html5-additional-options)
                           (current-buffer)
                           'no-mark
                           "*Org Pandoc Export Error Buffer"
                           t)
  (goto-char (point-min))
  (html-mode))

(provide 'ox-pandoc-html)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; ox-pandoc-html.el ends here
