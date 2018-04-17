

; c-mode customizations
(setq c-indent-level                  4)
(setq c-continued-statement-offset    4)
(setq c-argdecl-indent                4)
(setq c-brace-offset                 -4)
(setq c-label-offset                 -4)
(setq c-auto-newline                "t")


(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;;html-helper-mode
(add-to-list 'auto-mode-alist '("\\.shtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ssi$" . html-mode))

(setq html-helper-never-indent t)
;;disable auto-fill-mode and fly-spell-mode for html
(setq html-helper-mode-hook '(lambda()
                               (auto-fill-mode nil)
                               (local-set-key "t" 'self-insert-command)))


;;;(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode nil)


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)) auto-mode-alist))

;; Automatically go into mail-mode if filename starts with /tmp/mutt
(setq auto-mode-alist (append (list (cons "^\/tmp\/mutt" 'mail-mode))
			      auto-mode-alist))


(defun mail-subject-or-text ()
  "Move point to end of Subject field if subject is empty,
otherwise to the begining of text."
  (interactive)
  (mail-subject)
  (when (> (current-column) (length "Subject :")) (mail-text)))

(add-hook 'mail-mode-hook '(lambda()
			     (auto-fill-mode 1)
			     (setq desktop-save nil)
			     (set-input-method "hebrew-custom-lyx")
			     (setq ispell-local-dictionary "he")
			     (mail-subject-or-text)) t)


;; sort corrections by likeliness
(setq flyspell-sort-corrections nil)
;; (add-hook 'flyspell-mode-hook '(lambda()
;; 				 (define-key flyspell-mode-map "\C-cs" 'my-spell)
;; 				 (define-key flyspell-mode-map "\C-c," 'flyspell-goto-next-error)
;; 				 (define-key flyspell-mode-map "\C-cת" 'flyspell-goto-next-error)
;; 				 (define-key flyspell-mode-map "\C-c." 'flyspell-auto-correct-word)
;; 				 (define-key flyspell-mode-map "\C-cץ" 'flyspell-auto-correct-word)
;; 				 (define-key flyspell-mode-map "\C-c;" 'flyspell-auto-correct-previous-word)
;; 				 (define-key flyspell-mode-map "\C-cף" 'flyspell-auto-correct-previous-word)))


(add-hook 'dired-mode-hook '(lambda ()
			      (define-key dired-mode-map
				"\C-co"
				'dired-open-in-external-app)
			      ))

(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))
