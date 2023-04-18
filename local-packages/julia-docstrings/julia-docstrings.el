;;; julia-docstrings.el --- Functions to build Julia docstrings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ronan Arraes Jardim Chagas
;;
;; Author           : Ronan Arraes Jardim Chagas
;; Created          : April 2023
;; Keywords         : julia-mode julia-ts-mode
;; Package-Requires : (emacs "29")
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
;; This package contains functions to help creating Julia docstrings.

;;;; Code:

(defgroup julia-docstrings nil
  "Functions to help generating Julia docstrings."
  :prefix "julia-docstrings-"
  :group 'languages)

(defun julia-docstrings--create-function-declaration (name arguments keywords suffix)
  "Create declaration for a Julia function docstring.

NAME is the name of the function. ARGUMENTS are the arguments of the function.
KEYWORDS are the keywords of the function. SUFFIX is any information after the
function declaration, e.g., a `where' clause."

  ;; We start with `decl = <name>('.
  (let ((decl (concat (string-trim (string-replace "function" "" name)) "(")))
    ;; Concatenate the arguments.
    (when arguments
      (dolist (elem arguments)
        (setq decl (concat decl elem ", ")))

      ;; Remove the last ", " added by the loop.
      (setq decl (substring decl 0 -2)))

    ;; Add `kwargs...' if we have keywords.
    (when keywords
      (setq decl (concat decl "; kwargs...")))

    ;; Close the parenthesis and add the function suffix.
    (setq decl (string-trim (concat decl ") " suffix)))

    ;; Return the declaration.
    decl))

(defun julia-docstrings--create-macro-declaration (name arguments)
  "Create declaration for a Julia macro docstring.

NAME is the name of the macro. ARGUMENTS are the arguments of the macro."

  ;; We start with `decl = <name>('.
  (let ((decl (concat "@" (string-trim (string-replace "macro" "" name)) "(")))
    ;; Concatenate the arguments.
    (when arguments
      (dolist (elem arguments)
        (setq decl (concat decl elem ", ")))

      ;; Remove the last ", " add by the loop.
      (setq decl (substring decl 0 -2)))

    (setq decl (concat decl ")"))

    ;; Return the declaration.
    decl))

(defun julia-docstrings--get-argument-default-value (argument)
  "Return the ARGUMENT default value or nil if it is not set."
  (if (string-match "=\\(.*\\)" argument)
      (string-trim (match-string 1 argument))
    nil))

(defun julia-docstrings--get-declaration-type (decl)
  "Return the declaration type of DECL."
  (cond ((string-match-p "^[\s]*function" decl) 'function)
        ((string-match-p "^[\s]*macro" decl) 'macro)
        ((string-match-p "^[\s]*mutable struct" decl) 'struct)
        ((string-match-p "^[\s]*struct" decl) 'struct)
        ((string-match-p "^[\s]*abstract type" decl) 'abstract-type)
        (t 'unknown)))

(defun julia-docstrings--get-variable-list (str)
  "Return a list with the variables in STR."
  (let* ((variables (list))
         (tokens    (split-string str ","))
         (wait      nil)
         (aux       ""))

    ;; Loop through the tokens.
    (dolist (elem tokens)
      (unless (= (length elem) 0)
        (setq aux (concat aux (string-trim elem)))

        ;; If the token has `{' but do not have `}', it means that we are with a
        ;; variable like `a::Union{Nothing, Float64}'. Hence, we must concatenate
        ;; the string and wait for the next token that has `}'.
        (if (and (or (string-match-p "{" elem) wait)
                 (not (string-match-p "}" elem)))
            (progn (setq aux (concat aux ", "))
                   (setq wait t)
                   (setq add-element nil))

          (setq variables (append variables (list (string-trim aux))))
          (setq aux "")
          (setq wait nil))))

    variables))

(defun julia-docstrings--prepare-declaration (str)
  "Prepare the declaration in STR by trimming and removing all the newlines."
  (let* ((aux1 (replace-regexp-in-string "\n[\s]*" " " str)) ;
         (aux2 (replace-regexp-in-string "\s)" ")" aux1))
         (ret  (string-trim (replace-regexp-in-string "(\s" "(" aux2))))
    ret))

(defun julia-docstrings--process-declaration (decl)
  "Process the Julia declaration in DECL to split the declaration data.

This function returns a list of strings with the declaration name, arguments,
and keywords."
  (let* ((name       (progn (if (string-match "\\([^(]*\\)" decl)
                                (string-trim (match-string 1 decl))
                              "")))
         (parameters (progn (if (string-match "(\\(.*\\))" decl)
                                (string-trim (match-string 1 decl))
                              "")))
         (arguments  (progn (if (string-match "\\([^;]*\\)" parameters)
                                (string-trim (match-string 1 parameters))
                              "")))
         (keywords   (progn (if (string-match "[^;]*;\\(.*\\)" parameters)
                                (string-trim (match-string 1 parameters))
                              "")))
         (suffix   (progn (if (string-match ")[\s]*\\(.*\\)" decl)
                                (string-trim (match-string 1 decl))
                              ""))))
    (list name
          (julia-docstrings--get-variable-list arguments)
          (julia-docstrings--get-variable-list keywords)
          suffix)))

(defun julia-docstrings--remove-argument-default-value (argument)
  "Remove the default value of ARGUMENT if any."
  (string-trim (replace-regexp-in-string "[\s]*=.*" "" argument)))

;; =============================================================================
;;                      Functions Related with YASnippet
;; =============================================================================

(defun julia-docstrings--yas-docstring (str)
  "Create Julia docstring for YASnippet.

STR is the input text with the declaration."
  (let* ((ret "\"\"\"\n    ")
         (decl (julia-docstrings--prepare-declaration str))
         (type (julia-docstrings--get-declaration-type str)))

    (cond ((or (eq type 'function)
               (eq type 'macro))

           ;; Create the documentation for a function or a macro
           ;; ==================================================================

           (let* ((decl-info      (julia-docstrings--process-declaration decl))
                  (name           (nth 0 decl-info))
                  (arguments      (nth 1 decl-info))
                  (keywords       (nth 2 decl-info))
                  (suffix         (nth 3 decl-info))
                  (processed-decl (if (eq type 'function)
                                      (julia-docstrings--create-function-declaration name
                                                                                  arguments
                                                                                  keywords
                                                                                  suffix)
                                    (julia-docstrings--create-macro-declaration name arguments))))
             (setq ret (concat ret processed-decl "\n\n$1\n"))))

          (t

           ;; Fallback for types that are not supported yet
           ;; ==================================================================

           (setq ret (concat ret decl))))

     ;; We must concatenate the original string.
    (setq ret (concat ret "\"\"\"\n" str "$0"))

    ret))

(defun julia-docstrings--yas-docstring-with-arguments (str)
  "Create Julia docstring for YASnippet, listing the arguments.

STR is the input text with the declaration."
  (let* ((ret "\"\"\"\n    ")
         (decl (julia-docstrings--prepare-declaration str))
         (type (julia-docstrings--get-declaration-type str)))

    (cond ((or (eq type 'function)
               (eq type 'macro))

           ;; Create the documentation for a function or a macro
           ;; ==================================================================

           (let* ((decl-info      (julia-docstrings--process-declaration decl))
                  (name           (nth 0 decl-info))
                  (arguments      (nth 1 decl-info))
                  (keywords       (nth 2 decl-info))
                  (suffix         (nth 3 decl-info))
                  (processed-decl (if (eq type 'function)
                                      (julia-docstrings--create-function-declaration name
                                                                                  arguments
                                                                                  keywords
                                                                                  suffix)
                                    (julia-docstrings--create-macro-declaration name arguments)))
                  (snippet-id     2))

             (setq ret (concat ret processed-decl "\n\n$1\n\n# Arguments\n\n"))

             (dolist (argument arguments)
               (setq ret (concat ret
                                 "- \\`"
                                 (julia-docstrings--remove-argument-default-value argument)
                                 "\\`: $"
                                 (number-to-string snippet-id)
                                 "\n"))

               (setq snippet-id (+ snippet-id 1)) ;

               (let ((default-value (julia-docstrings--get-argument-default-value argument)))
                 (when default-value
                   (setq ret (concat ret "    (**Default** = " default-value ")")))))))

          (t

           ;; Fallback for types that are not supported yet
           ;; ==================================================================

           (setq ret (concat ret decl))))

     ;; We must concatenate the original string.
    (setq ret (concat ret "\"\"\"\n" str "$0"))

    ret))

(defun julia-docstrings--yas-docstring-with-arguments-and-keywords (str)
  "Create Julia docstring for YASnippet, listing the arguments and keywords.

STR is the input text with the declaration."
  (let* ((ret "\"\"\"\n    ")
         (decl (julia-docstrings--prepare-declaration str))
         (type (julia-docstrings--get-declaration-type str)))

    (cond ((or (eq type 'function)
               (eq type 'macro))

           ;; Create the documentation for a function or a macro
           ;; ==================================================================

           (let* ((decl-info      (julia-docstrings--process-declaration decl))
                  (name           (nth 0 decl-info))
                  (arguments      (nth 1 decl-info))
                  (keywords       (nth 2 decl-info))
                  (suffix         (nth 3 decl-info))
                  (processed-decl (if (eq type 'function)
                                      (julia-docstrings--create-function-declaration name
                                                                                  arguments
                                                                                  keywords
                                                                                  suffix)
                                    (julia-docstrings--create-macro-declaration name arguments)))
                  (snippet-id     2))

             (setq ret (concat ret processed-decl "\n\n$1\n\n# Arguments\n\n"))

             (dolist (argument arguments)
               (setq ret (concat ret
                                 "- \\`"
                                 (julia-docstrings--remove-argument-default-value argument)
                                 "\\`: $"
                                 (number-to-string snippet-id)
                                 "\n"))

               (setq snippet-id (+ snippet-id 1)) ;

               (let ((default-value (julia-docstrings--get-argument-default-value argument)))
                 (when default-value
                   (setq ret (concat ret "    (**Default** = " default-value ")\n")))))

             ;; For functions, we should also add the keywords, if any.
             (when (and (eq type 'function) keywords)
               (setq ret (concat ret "\n# Keywords\n\n"))

               (dolist (keyword keywords)
                 (setq ret (concat ret
                                   "- \\`"
                                   (julia-docstrings--remove-argument-default-value keyword)
                                   "\\`: $"
                                   (number-to-string snippet-id)
                                   "\n"))

                 (setq snippet-id (+ snippet-id 1)) ;

                 (let ((default-value (julia-docstrings--get-argument-default-value keyword)))
                   (when default-value
                     (setq ret (concat ret "    (**Default** = " default-value ")\n"))))))))

          (t

           ;; Fallback for types that are not supported yet
           ;; ==================================================================

           (setq ret (concat ret decl))))

     ;; We must concatenate the original string.
    (setq ret (concat ret "\"\"\"\n" str "$0"))

    ret))

(provide 'julia-docstrings)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; julia-docstrings.el ends here
