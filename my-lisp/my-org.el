

;; org-mode settings
(setq org-log-done t)
(setq org-agenda-files (list "~/documents/org/personal.org"))
(setq org-agenda-start-on-weekday 0)
; I prefer return to activate a link
(setq org-return-follows-link t)

;; (setq org-agenda-custom-commands
;;     '(("w" todo "WAITING" nil)
;;     ("n" todo "NEXT" nil)
;;     ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
;; )

(defun gtd ()
  "Open the main org file."
   (interactive)
   (find-file "~/documents/org/main.org")
)

(setq org-todo-keywords
      '((sequence
	 "מצב:TODO" "מצב:DONE"
	 )))


