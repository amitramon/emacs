(defun my-server-start()
  "Start emacs server"
  (interactive)
  (server-start))



;; (add-hook 'server-switch-hook
;; 	  (lambda nil
;; 	    (let ((server-buf (current-buffer)))
;; 	      (bury-buffer)
;; 	      (switch-to-buffer-other-frame server-buf))))

;; (add-hook 'server-done-hook 'delete-frame)
;; (add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;; (setq server-switch-hook nil)
;; (setq server-done-hook nil)
