;;
;;

;; t is the default value
;; (setq desktop-restore-frames t)

;; default: ("~/.emacs.d" "~")
;; (setq desktop-path '("." "~"))



;; (setq your-own-path default-directory)

;; (if (file-exists-p
;;      (concat your-own-path ".emacs.desktop"))
;;     (desktop-read your-own-path))

;; (add-hook 'kill-emacs-hook
;; 	  `(lambda ()
;; 	     (desktop-save ,your-own-path t)))


;; default: nil
;; (desktop-save-mode 1)		; save emacs session
;; (desktop-save-mode 0)

;;; following settings are now set by customization

;; (setq desktop-buffers-not-to-save
;;       (concat "\\("
;; 	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;; 	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;; 	      "\\)$"))

;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

