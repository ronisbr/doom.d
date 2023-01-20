;;; julia-local.el --- Local functions for Julia mode -*- lexical-binding: t; -*-

;; Functions related to snippets
;; =============================================================================

;; This function transforms, for example:
;;
;; function test(a,
;;               b,
;;               c)
;;
;; into
;;
;; test(a, b, c)
;;
;; and
;;
;; macro test(a,
;;            b,
;;            c)
;;
;; into
;;
;;     @test(a, b, c)

(defun ronisbr/julia--decl-for-doc (str)
  "Create the documentation string of a function/macro in `str`."

  ;; `ret` will store the text to be returned.
  (let ((ret ""))

    ;; Everything must be in one line.
    (let* ((aux1 (replace-regexp-in-string "\n[\s]*" " " str))
           (aux2 (replace-regexp-in-string "\s)" ")" aux1))
           (tmp  (replace-regexp-in-string "(\s" "(" aux2)))

        ;; Check if the declaration is a function.
        (if (string-prefix-p "function" tmp)
            (setq ret (concat ret (string-trim (substring tmp 8 nil))))

          ;; Check if the declaration is a macro.
          (if (string-prefix-p "macro" tmp)
              (progn
                (setq ret (concat ret "@"))
                (setq ret (concat ret (string-trim (substring tmp 5 nil)))))

            ;; It is neither a function nor a macro.
            (setq ret (concat ret (string-trim tmp))))))))

;; This function creates the documentation of a function or macro with the
;; arguments. It converts:
;;
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)
;;
;; into
;;
;; """
;;     test(a, b, c)
;;
;; $1
;;
;; """
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)$0
;;
;; which must be used together the function `yas-snippet-expand`.

(defun ronisbr/julia--create-doc (str)
  "Create the Julia documentation text for a function/macro call in `str`."

  ;; `ret` will store the text to be returned.
  (let ((ret "\"\"\"\n    "))

    ;; Get the function declaration for the documentation.
    (setq ret (concat ret
                      (ronisbr/julia--decl-for-doc str)
                      "\n\n$1\n\"\"\"\n" str "$0"))))

;; This function creates the documentation of a function or macro with the
;; arguments. It converts:
;;
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)
;;
;; into
;;
;; """
;;     test(a, b, c)
;;
;; $1
;;
;; # Args
;;
;; * `a`: $2
;; * `b`: $3
;; * `c`: $4
;;
;; """
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)$0
;;
;; which must be used together the function `yas-snippet-expand`.

(defun ronisbr/julia--create-doc-with-args (str)
  "Create the Julia documentation text for a function/macro call in `str` that
includes the arguments."

  ;; `ret` will store the text to be returned. Initially, we should wrap the
  ;; function call in one line.
  (let ((ret "\"\"\"\n    "))
    ;; Get the function declaration for documentation.
    (setq ret (concat ret
                      (ronisbr/julia--decl-for-doc str)
                      "\n\n$1\n\n"))

    ;; Now, we must extract the name of the arguments.
    (let* ((ret_copy ret)
           (args (progn
                   (string-match "(\\(.*\\))" ret_copy)
                   (match-string 1 ret_copy)))
           (i 2))
      ;; Check if the function has any arguments.
      (when (not (= (length args) 0))
            (let ((list (split-string args ",")))
              (setq ret (concat ret "# Args\n\n"))
              (while list
                (let ((arg (string-trim (car list))))
                  (setq ret (concat ret (format "- \\`%s\\`: $%d\n" arg i))))
                (setq i (+ i 1))
                (setq list (cdr list)))
              (setq ret (concat ret "\n"))))

      (setq ret (concat ret "\"\"\"\n" str "$0")))))

;; This function creates the documentation of a function or macro with the
;; arguments and return values. It converts:
;;
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)
;;
;; into
;;
;; """
;;     test(a, b, c)
;;
;; $1
;;
;; # Args
;;
;; * `a`: $2
;; * `b`: $3
;; * `c`: $4
;;
;; # Returns
;;
;; $5
;;
;; """
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)$0
;;
;; which must be used together the function `yas-snippet-expand`.

(defun ronisbr/julia--create-doc-with-args-and-ret (str)
  "Create the Julia documentation text for a function/macro call in `str` that
includes the arguments and return values."

  ;; `ret` will store the text to be returned. Initially, we should wrap the
  ;; function call in one line.
  (let ((ret "\"\"\"\n    "))
    ;; Get the function declaration for documentation.
    (setq ret (concat ret
                      (ronisbr/julia--decl-for-doc str)
                      "\n\n$1\n\n"))

    ;; Now, we must extract the name of the arguments.
    (let* ((ret_copy ret)
           (args (progn
                   (string-match "(\\(.*\\))" ret_copy)
                   (match-string 1 ret_copy)))
           (i 2))
      ;; Check if the function has any arguments.
      (when (not (= (length args) 0))
            (let ((list (split-string args ",")))
              (setq ret (concat ret "# Args\n\n"))
              (while list
                (let ((arg (string-trim
                            (replace-regexp-in-string "::.*" "" (car list)))))
                  (setq ret (concat ret (format "* %s: $%d\n" arg i))))
                (setq i (+ i 1))
                (setq list (cdr list)))
              (setq ret (concat ret "\n"))))

      (setq ret (concat ret
                        (format "# Returns\n\n$%d\n\n\"\"\"\n%s$0"
                                i str))))))

;; This function creates the documentation of a function or macro with the
;; return values. It converts:
;;
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)
;;
;; into
;;
;; """
;;     test(a, b, c)
;;
;; $1
;;
;; # Returns
;;
;; """
;; function test(a::Int64,
;;               b::Bool,
;;               c::Float64)$0
;;
;; which must be used together the function `yas-snippet-expand`.

(defun ronisbr/julia--create-doc-with-ret (str)
  "Create the Julia documentation text for a function/macro call in `str`."

  ;; `ret` will store the text to be returned.
  (let ((ret "\"\"\"\n    "))

    ;; Get the function declaration for the documentation.
    (setq ret (concat ret
                      (ronisbr/julia--decl-for-doc str)
                      "\n\n$1\n\n# Returns\n\n$2\n\n\"\"\"\n" str "$0"))))

(provide 'julia-local)
