

; c-mode customizations
(setq c-indent-level                  4)
(setq c-continued-statement-offset    4)
(setq c-argdecl-indent                4)
(setq c-brace-offset                 -4)
(setq c-label-offset                 -4)
(setq c-auto-newline                "t")

;;; OpenGL GLSL shader files
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.gs\\'" . c-mode))


(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;;html-helper-mode
;; (add-to-list 'auto-mode-alist '("\\.shtml$" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.ssi$" . html-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.shtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ssi$" . web-mode))



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

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)) auto-mode-alist))

;; mutt and neomutt mail
(add-to-list 'auto-mode-alist '("^/tmp/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("^/tmp/neomutt" . mail-mode))


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

;;; xkcd-rand is originaly bound to "r", but "r" is masked
;;; by imagemagick
(add-hook 'xkcd-mode-hook '(lambda ()
			     (define-key xkcd-mode-map
			       (kbd "R")
			       'xkcd-rand)
			     (define-key xkcd-mode-map
			       (kbd "G")
			       'xkcd-get)
			     ))

