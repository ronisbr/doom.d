;;; write-mode.el --- Write mode to Doom Emacs . -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ronan Arraes Jardim Chagas
;;
;; Author           : Ronan Arraes Jardim Chagas
;; Created          : January 2023
;; Keywords         : doom writeroom
;; Package-Requires : ((emacs "29") (writeroom-mode "3"))
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
;; This package contains functions enable and disable Ronis_BR's write mode,
;; which changes the theme and font of Doom Emacs.

(require 'writeroom-mode)

;;;; Code:

(defgroup write-mode nil
  "Options related with the write mode."
  :tag "Ronis_BR's write mode"
  :prefix "write-mode-"
  :group 'write-mode)

(defcustom write-mode-font-face
  '(:family "iA Writer Duospace")
  "Font face to be used in the buffer where the write mode is enabled."
  :group 'display
  :type '(plist :tag "Face property list")
  :version "29.1")

(defcustom write-mode-theme
  'doom-plain
  "Theme for the write mode."
  :group 'display
  :type 'symbol
  :version "29.1")

(defvar write-mode--default-theme
  'doom-one
  "Variable to store the doom theme before setting the write mode.")

(defvar write-mode--default-font
  doom-font
  "Variable to store the doom font before setting the write mode.")

(defvar write-mode--enabled
  nil
  "Variable to store whether the write mode is enabled.")

(defun write-mode--enable ()
  "Enable the write mode."
  (+zen/toggle 1)
  (setq write-mode--default-font doom-font)
  (setq write-mode--default-theme doom-theme)
  (setq doom-font (apply 'font-spec write-mode-font-face))
  (setq doom-theme 'doom-plain)

  (display-line-numbers-mode -1)
  (load-theme doom-theme t)
  (doom/reload-font)
  (setq write-mode--enabled t))

(defun write-mode--disable ()
  "Disable the write mode"
  (+zen/toggle -1)
  (setq doom-theme write-mode--default-theme)
  (setq doom-font write-mode--default-font)

  (display-line-numbers-mode 1)
  (load-theme doom-theme t)
  (doom/reload-font)
  (setq write-mode--enabled nil))

;;;###autoload
(defun write-mode-toogle ()
  "Toggle the write mode."
  (interactive)
  (if write-mode--enabled
      (write-mode--disable)
    (write-mode--enable)))

(provide 'write-mode)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; write-mode.el ends here
