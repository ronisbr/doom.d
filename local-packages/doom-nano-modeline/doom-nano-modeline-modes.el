;;; doom-nano-modeline-modes.el --- Modes for the doom-nano-modeline -*- lexical-binding: t; -*-

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
;; This file contains the functions required to register the modes in doom-nano
;; modeline.

;;; Code

;; Default mode
;; =============================================================================

(defun doom-nano-modeline-default-mode ()
  (doom-nano-modeline--render (doom-nano-modeline-buffer-name-vc-and-major-mode)
                              (append (doom-nano-modeline-cursor-position)
                                      (doom-nano-modeline--space)
                                      (doom-nano-modeline-org-clock-timer))))

;; Magit
;; =============================================================================

(defun doom-nano-modeline--magit-status-mode-p ()
  "Return t if we are in `magit-status-mode' or nil otherwise."
  (derived-mode-p 'magit-status-mode))

(defun doom-nano-modeline--magit-status-mode ()
  "Render the modeline in `magit-status-mode'"
    (doom-nano-modeline--render
     `(("Magit:" . doom-nano-modeline-major-mode-face)
       (" " . nil)
       (,(file-name-nondirectory
          (directory-file-name
           (file-name-directory default-directory))) . nil)
       (" " . nil)
       (,(concat "[#" (magit-get-current-branch) "]") . doom-nano-modeline-vc-branch-name-face))
     nil
     t))

;; Messages
;; =============================================================================

(defun doom-nano-modeline--messages-buffer-mode-p ()
  "Return t if we are in `messages-buffer-mode' or nil otherwise."
  (derived-mode-p 'messages-buffer-mode))

(defun doom-nano-modeline--messages-buffer-mode ()
    (doom-nano-modeline--render
     `(("Messages" . nil))
     nil
     t))

;; Org
;; =============================================================================

(defun doom-nano-modeline--org-mode-p ()
  "Return t if we are in `org-mode' or nil otherwise."
  (derived-mode-p 'org-mode))

(defun doom-nano-modeline--org-mode ()
  "Render the modeline if `org-mode'."
  (doom-nano-modeline--render
   (doom-nano-modeline-org-mode-buffer-name-and-major-mode)
   (append (doom-nano-modeline-cursor-position)
           (doom-nano-modeline--space)
           (doom-nano-modeline-org-clock-timer))))

;; Org-agenda
;; =============================================================================

(defun doom-nano-modeline--org-agenda-mode-p ()
  "Return t if we are in `org-agenda-mode' or nil otherwise."
  (derived-mode-p 'org-agenda-mode))

(defun doom-nano-modeline--org-agenda-mode ()
  "Render the modeline if `org-agenda-mode'."
  (doom-nano-modeline--render
   `(("Agenda" . 'doom-nano-modeline-major-mode-face))
   `((,(format-time-string "%A %-e %B %Y") . nil))
   t))

;; Org-capture
;; =============================================================================

(defun doom-nano-modeline--org-capture-mode-p ()
  "Return t if we are in `org-capture-mode' of nil otherwise."
  (bound-and-true-p org-capture-mode))

(defun doom-nano-modeline--org-capture-mode ()
  "Render the modeline in `org-capture-mode'."
  (doom-nano-modeline--render
   `(("Capture" . doom-nano-modeline-major-mode-face)
     (,(concat " (" (org-capture-get :description) ")") . nil)
     (" --- Finish: C-c C-c, refile: C-c C-w, cancel: C-c C-k" . font-lock-comment-face))
   nil))

(defun doom-nano-modeline--org-capture-mode-turn-off-header-line ()
  "Turn off the header-line in `org-capture-mode' if the doom-nano modeline position is 'top."
  (when (eq doom-nano-modeline-position 'top)
    (setq-local header-line-format (default-value 'header-line-format))
    (message nil)))

(defun doom-nano-modeline--org-capture-mode-on-activate ()
  "Add hooks to `org-capture-mode' related with the doom-nano modeline."
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'doom-nano-modeline--org-capture-mode-turn-off-header-line
              90)))

(defun doom-nano-modeline--org-capture-mode-on-inactivate ()
  "Remove hooks to `org-capture-mode' related with the doom-nano modeline."
  (remove-hook 'org-capture-mode-hook
               #'doom-nano-modeline--org-capture-mode-turn-off-header-line))

;; Special mode
;; =============================================================================

(defun doom-nano-modeline--special-mode-p ()
  "Return t if we are in `special-mode' or nil otherwise."
  (derived-mode-p 'special-mode))

(defun doom-nano-modeline--special-mode ()
  "Render the modeline in `special-mode'."
    (doom-nano-modeline--render
     `((,(format-mode-line "%b") . nil))
     nil
     t))

;; Vterm
;; =============================================================================

(defun doom-nano-modeline--vterm-mode-p ()
  "Return t if we are in `vterm-mode' or nil otherwise."
  (derived-mode-p 'vterm-mode))

(defun doom-nano-modeline--vterm-mode ()
  "Render the modeline in `vterm-mode'."
  (doom-nano-modeline--render
   `(;; The name of the shell.
     (,(concat "vterm [" (file-name-base vterm-shell) "]") . doom-nano-modeline-major-mode-face)
     (" " . nil)
     ;; The abbreviated directory.
     (,(abbreviate-file-name default-directory)))
   nil
   t))

(defun doom-nano-modeline--vterm-mode-on-activate ()
  "Advice the function `vterm--set-title' to track the current directory."
  (with-eval-after-load 'vterm
    (advice-add 'vterm--set-title :before #'doom-nano-modeline--vterm-set-title-advice)))

(defun doom-nano-modeline--vterm-mode-on-inactivate ()
  "Remove advice of the function `vterm--set-title'."
  (advice-remove 'vterm--set-title #'doom-nano-modeline--vterm-set-title-advice))

(provide 'doom-nano-modeline-modes)
