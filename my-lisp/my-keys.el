;; (define-key global-map "\C-x\C-m"   'exwecute-extended-command)
;; (define-key global-map "\C-x\C-k"   'kill-region) ; default bind: edit-kbd-macro
;; (define-key global-map "\C-w"       'backward-kill-word) ; default bind: kill-region

(global-set-key (kbd "C-x C-b") 'ibuffer) ; default bind: buffer-list

(global-set-key (kbd "C-c t") 'beginning-of-buffer)
(global-set-key (kbd "C-c b") 'end-of-buffer)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c w") 'woman)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c q") 'query-replace)


(global-set-key (kbd "C-c f") 'recentf-open-files)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; delete-backward-char DEL
;; backward-kill-word M-DEL

;; delete-forward-char C-d
;; kill-word M-d

(global-set-key (kbd "<C-f8>") 'first-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<S-f8>") 'previous-error)
;; (global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'scroll-right)
(global-set-key (kbd "<f7>") 'scroll-left)
(global-set-key (kbd "<f9>") 'compile)
;; (global-set-key [f10]             'start-kbd-macro)
;; (global-set-key [f11]             'end-kbd-macro)
;; (global-set-key [f12]             'call-last-kbd-macro)
(global-set-key (kbd "<kp-add>") 'enlarge-window)
(global-set-key (kbd "<kp-subtract>") 'shrink-window)
(global-set-key (kbd "<kp-decimal>") 'what-cursor-position)

(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<C-f5>") 'run-python)
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'rgrep) ; replace 2-column commands (bound also to C-x 6)
(global-set-key (kbd "M-o") 'other-window) ; replace 'set-face-...

;;; replace delete-other-windows
(global-set-key (kbd "C-x 1") 'delete-other-windows-vertically)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun change-dict-he()
  "Change to Hebrew dictionary"
  (interactive)
  (ispell-change-dictionary "he"))

(defun change-dict-en()
  "Change to English dictionary"
  (interactive)
  (ispell-change-dictionary "english"))

;; (global-set-key (kbd "C-c d h") '(()
;; 				"Change to Hebrew dictionary"
;; 				(interactive)
;; 				(ispell-change-dictionary "he")))

(global-set-key (kbd "C-c d h") 'change-dict-he)
(global-set-key (kbd "C-c d e") 'change-dict-en)

(global-set-key (kbd "C-c d d") 'dictionary-search)
(global-set-key (kbd "C-c d m") 'dictionary-match-words)
(global-set-key (kbd "C-c d s") 'ispell-word)


(global-set-key (kbd "<M-down>") '(lambda()
				    "scroll down, cursor remains in place"
				    (interactive)
				    (scroll-up 1)
				    (next-line 1)))

(global-set-key (kbd "<M-up>") '(lambda()
				  "scroll up, cursor remains in place"
				  (interactive)
				  (scroll-up -1)
				  (next-line -1)))

(global-set-key [mouse-5] '(lambda()
			     "scroll down one line"
			     (interactive)
			     (scroll-down -1)))

(global-set-key [mouse-4] '(lambda()
			     "scroll up one line"
			     (interactive)
			     (scroll-down 1)))

(defvar my-bidi-paragraph-ordering 0
  "saves the state of `bidi-paragraph-direction' per buffer")
(make-variable-buffer-local 'my-bidi-paragraph-ordering)

(defun toggle-paragraph-bidi-ordering()
  "Toggles the values of `bidi-paragraph-direction'"
  (interactive)
  (setq my-bidi-paragraph-ordering
	(% (1+ my-bidi-paragraph-ordering) 3))
  (setq bidi-paragraph-direction
	(nth my-bidi-paragraph-ordering '(nil right-to-left left-to-right)))
  (message "bidi paragraph direction set to %s" bidi-paragraph-direction))


(global-set-key (kbd "C-c C-h") 'toggle-paragraph-bidi-ordering)

;; (global-set-key (kbd "C-c C-h") '(lambda()
;; 				   "Force right-to-left paragraph ordering"
;; 				   (interactive)
;; 				   (setq bidi-paragraph-direction 'right-to-left)))

;; (global-set-key (kbd "C-c C-g") '(lambda()
;; 				   "Force left-to-right paragraph ordering"
;; 				   (interactive)
;; 				   (setq bidi-paragraph-direction 'left-to-right)))

(defun set_bidi_on()
  "Set bidi reordering"
  (interactive)
  (setq bidi-display-reordering t)
  (redraw-display))

(defun set_bidi_of()
  "Unset bidi reordering"
  (interactive)
  (setq bidi-display-reordering nil)
  (redraw-display))


;; (global-set-key [M-f12] set_bidi_on())

;; (global-set-key [M-f11] set_bidi_of())


;; ar: doesn't work... at least doesn't change the language for flyspell.
(setq-default ispell-dictionary "english")
(defun my-switch-dictionarry()
  (interactive)
  (let* ((dic (if (boundp 'ispell-local-dictionary) ispell-local-dictionary ispell-dictionary))
	 (change (if (string= dic "he") "english" "he")))
    (set (make-local-variable 'ispell-local-dictionary) change)
    (message "Dictionary switched to %s" change)
    ))

(global-set-key (kbd "C-c d c") 'my-switch-dictionarry)
;; (global-set-key '[(f8)]		'my-switch-dictionarry)

;;; https://github.com/alezost/mwim.el
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)
;; (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
;; (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
;; (global-set-key (kbd "<home>") 'mwim-beginning-of-line-or-code)
;; (global-set-key (kbd "<end>") 'mwim-end-of-line-or-code)
(global-set-key (kbd "<C-tab>") #'mwim)
