
(setq frame-background-mode 'dark) ; tell rst about my background
(require 'font-lock)
(load-library "rst")	     ; reStructures text
(add-hook 'rst-mode-hook
	  '(lambda ()
	     (turn-on-font-lock)
	     (electric-indent-local-mode -1)
	     ;(set-face-foreground 'rst-comment-face   "magenta")
	     ;(set-face-foreground 'rst-directive-face   "blue")
	     ;(set-face-foreground 'font-lock-function-name-face   "red")
	     ;(set-face-foreground 'font-lock-comment-face   "green")
	     ;; (message "I am here")
	     ))

;; Note: electric-indent-mode seems to be causing indentation problems
;; with rst-mode


(setq rst-compile-toolsets
  '((html . ("rst2html" ".html" nil))
    (latex . ("rst2latex" ".tex" nil))
    (newlatex . ("rst2xetex" ".tex" nil))
    (pseudoxml . ("rst2pseudoxml" ".xml" nil))
    (xml . ("rst2xml" ".xml" nil))
    (pdf . ("rst2pdf" ".pdf" nil))
    (s5 . ("rst2s5" ".xml" nil))))
