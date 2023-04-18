;;; ~/.doom.d/settings/setup-auctex.el --- Configure AUXTeX -*- lexical-binding: t; -*-

(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin")

(after! tex
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-method (quote synctex))
  (setq TeX-view-program-list
        '(("Preview.app" "open -a Preview.app %o")
          ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
          ("open" "open %o"))
        TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "Skim")
          (output-html "open"))))

(add-hook! LaTeX-mode (setq-local display-line-numbers nil))

(provide 'setup-auctex)
