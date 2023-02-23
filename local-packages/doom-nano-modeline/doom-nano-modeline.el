;;; doom-nano-modeline.el --- Modeline for Doom Emacs based on N Λ N O -*- lexical-binding: t; -*-

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
;;; Usage:
;; 1. Clone this repository to a directory.
;; 2. Put the following code in your .emacs, site-load.el, or other relevant
;;    file:
;; (add-to-list 'load-path "path-to-doom-nano-modeline")
;; (require 'doom-nano-modeline)
;; (doom-nano-modeline-mode)
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
;; This package provides an alternative modeline to Doom Emacs that is highly
;; based on N Λ N O modeline.

;;; Code

(require 'doom-themes)
(require 'doom-nano-modeline-core)
(require 'doom-nano-modeline-misc)
(require 'doom-nano-modeline-modes)

(defgroup doom-nano-modeline nil
  "Doom N Λ N O Modeline"
  :group 'doom)

(defcustom doom-nano-modeline-position 'top
  "Default position of the modeline (top or bottom)."
  :type '(choice (const :tag "Top"    top)
                 (const :tag "Bottom" bottom)))

(defcustom doom-nano-modeline-default-mode-format 'doom-nano-modeline-default-mode
  "Default mode to evaluate if no match was found in `doom-nano-modeline-mode-formats'."
  :type 'function)

(defcustom doom-nano-modeline-mode-format-activate-hook nil
  "Add hooks on mode activation."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode))

(defcustom doom-nano-modeline-mode-format-inactivate-hook nil
  "Add hooks on mode inactivation."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode))

(defcustom doom-nano-modeline-mode-formats
  '((magit-status-mode :mode-p doom-nano-modeline--magit-status-mode-p
                       :format doom-nano-modeline--magit-status-mode)

    (org-capture-mode :mode-p        doom-nano-modeline--org-capture-mode-p
                      :format        doom-nano-modeline--org-capture-mode
                      :on-activate   doom-nano-modeline--org-capture-mode-on-activate
                      :on-inactivate doom-nano-modeline--org-capture-mode-on-inactivate)

    (org-agenda-mode :mode-p doom-nano-modeline--org-agenda-mode-p
                     :format doom-nano-modeline--org-agenda-mode)

    (org-tags-buffer :mode-p doom-nano-modeline--org-tags-buffer-p
                     :format doom-nano-modeline--org-tags-buffer)

    (org-mode :mode-p doom-nano-modeline--org-mode-p
              :format doom-nano-modeline--org-mode)

    (messages-buffer-mode :mode-p doom-nano-modeline--messages-buffer-mode-p
                          :format doom-nano-modeline--messages-buffer-mode)

    (special-mode :mode-p doom-nano-modeline--special-mode-p
                  :format doom-nano-modeline--special-mode)

    (vterm-mode :mode-p        doom-nano-modeline--vterm-mode-p
                :format        doom-nano-modeline--vterm-mode
                :on-activate   doom-nano-modeline--vterm-mode-on-activate
                :on-inactivate doom-nano-modeline--vterm-mode-on-inactivate)

    (prog-mode :mode-p (lambda () t)
               :format doom-nano-modeline-default-mode))

  "Formats for doom-nano modeline depending on the current major mode."

  :type '(alist
          :key-type symbol
          :value-type (plist :key-type (choice (const :mode-p)
                                               (const :format)
                                               (const :on-activate)
                                               (const :on-inactivate)))))

(defcustom doom-nano-modeline-bottom-padding 0.25
  "Bottom padding in the doom-nano modeline."
  :type 'float)

(defcustom doom-nano-modeline-top-padding 0.20
  "Top padding in the doom-nano modeline."
  :type 'float)

(defface doom-nano-modeline-active-face
  '((t (:inherit mode-line)))
  "Face used when the modeline is active.")

(defface doom-nano-modeline-cursor-position-face
  '((t (:inherit font-lock-comment-face)))
  "Face for the cursor position shown to the right of the mode-line.")

(defface doom-nano-modeline-evil-emacs-state-face
  '((t (:inherit (font-lock-builtin-face bold))))
  "Face for the evil state indicator when in emacs state.")

(defface doom-nano-modeline-evil-insert-state-face
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face for the evil state indicator when in insert state.")

(defface doom-nano-modeline-evil-motion-state-face
  '((t (:inherit (font-lock-doc-face bold) :slant normal)))
  "Face for the evil state indicator when in motion state.")

(defface doom-nano-modeline-evil-normal-state-face
  '((t (:inherit (success bold))))
  "Face for the evil state indicator when in normal state.")

(defface doom-nano-modeline-evil-operator-state-face
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face for the evil state indicator when in operator state.")

(defface doom-nano-modeline-evil-replace-state-face
  '((t (:inherit (error bold))))
  "Face for the evil state indicator when in replace state.")

(defface doom-nano-modeline-evil-visual-state-face
  '((t (:inherit (warning bold))))
  "Face for the evil state indicator when in visual state.")

(defface doom-nano-modeline-inactive-face
  '((t (:inherit (font-lock-comment-face mode-line-inactive))))
  "Face used when the modeline is inactive.")

(defface doom-nano-modeline-macro-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for the macro recording indicator.")

(defface doom-nano-modeline-major-mode-face
  '((t (:inherit mode-line-emphasis)))
  "Face for the major mode.")

(defface doom-nano-modeline-org-clock-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for the org-clock.")

(defface doom-nano-modeline-vc-branch-name-face
  '((t (:inherit font-lock-comment-face)))
  "Face for the version control branch name.")

;; Internal variables.
(defvar doom-nano-modeline--saved-mode-line-format nil)
(defvar doom-nano-modeline--saved-header-line-format nil)
(defvar doom-nano-modeline--selected-window nil)

;;;###autoload
(define-minor-mode doom-nano-modeline-mode
  "Toggle the doom-nano-modeline minor mode"
  :global t
  :init-value nil

  (if doom-nano-modeline-mode
      (doom-nano-modeline-mode--activate)
    (doom-nano-modeline-mode--inactivate))

  ;; Run any registered hooks
  (run-hooks 'doom-nano-modeline-mode-hook))

(provide 'doom-nano-modeline)
