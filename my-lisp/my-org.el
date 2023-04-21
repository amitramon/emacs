;; org-mode settings
(require 'org)
(setq org-log-done t)

;; (setq org-agenda-files (list "~/org/personal.org"
;; 			     "~/org/notes.org"))

(setq org-agenda-files
      (list
       "~/org/notes.org"
       "~/dev/localize/da_geocomp_light_and_view/docker_apps/doc/todo.org"
       ))

(setq org-agenda-start-on-weekday 0)
; I prefer return to activate a link
(setq org-return-follows-link t)

(setq org-use-sub-superscripts '{})

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (python . t) (R . t)))

;; stop emacs asking for confirmation
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)

;; (setq org-agenda-custom-commands
;;     '(("w" todo "WAITING" nil)
;;     ("n" todo "NEXT" nil)
;;     ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
;; )

(defun gtd ()
  "Open the main org file."
   (interactive)
   (find-file "~/org/main.org")
)

;; (setq org-todo-keywords
;;       '((sequence
;; 	 "מצב:TODO" "מצב:DONE"
;; 	 )))

(add-hook 'org-mode-hook
	  '(lambda ()
	     (setq bidi-paragraph-direction nil)
	     ))

