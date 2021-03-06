;;; ~/.doom.d/settings/setup-keybindings.el -*- lexical-binding: t; -*-
;;
;; Configure global keybindings.

(map!
 "<f5>" #'neotree-toggle
 "<f6>" #'ronisbr/flyspell-switch-dictionary)

(map! :leader
      "<down>"  #'windmove-down
      "<left>"  #'windmove-left
      "<right>" #'windmove-right
      "<up>"    #'windmove-up)

(map! :leader
      (:prefix ("a" . "align")
       (:prefix ("c" . "comment")
        :desc "Right align comment"  "r" #'ronisbr/comment-right-align
        :desc "Center comment"       "c" #'ronisbr/comment-center-align)
       (:prefix ("t" . "text")
        :desc "Right align text"  "r" #'ronisbr/text-right-align
        :desc "Center text"       "c" #'ronisbr/text-center-align))
      (:prefix ("l" . "fill")
       :desc "Copy character"   "c" #'ronisbr/copy-to-end
       :desc "Fill with char."  "f" #'ronisbr/fill-to-end
       :desc "Fill entire line" "l" #'ronisbr/fill-line)
      (:prefix ("o" . "open")
       :desc "Org directory" "o" (lambda () (interactive) (dired ronisbr/org-directory))))

(map! :nv "<down>" #'evil-next-visual-line
      :nv "<up>"   #'evil-previous-visual-line)

(provide 'setup-keybindings)
