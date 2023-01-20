;;; render-org-to-html.el --- Render org files to HTML and show using Webkit -*- lexical-binding: t; -*-

(require 'ox-html)

;;;###autoload
(defun ronisbr/render-org-to-html ()
  "Render an org file to HTML and show using Webkit."
  (interactive)
  (let* ((expfile (make-temp-file "org-mode-html" nil ".html"))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'html expfile)
    (xwidget-webkit-browse-url (concat "file://" expfile))))

(provide 'render-org-to-html)
