

(setq desktop-restore-frames nil)

(setq your-own-path default-directory)
(if (file-exists-p
     (concat your-own-path ".emacs.desktop"))
    (desktop-read your-own-path))

(add-hook 'kill-emacs-hook
	  `(lambda ()
	     (desktop-save ,your-own-path t)))


;; (desktop-save-mode 1)		; save emacs session


(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
