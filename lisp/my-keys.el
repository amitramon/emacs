;; (define-key global-map "\C-x\C-m"   'exwecute-extended-command)
;; (define-key global-map "\C-x\C-k"   'kill-region) ; default bind: edit-kbd-macro
;; (define-key global-map "\C-w"       'backward-kill-word) ; default bind: kill-region

(global-set-key (kbd "C-x C-b") 'buffer-menu) ; default bind: buffer-list

(global-set-key (kbd "C-c t") 'beginning-of-buffer)
(global-set-key (kbd "C-c e") 'end-of-buffer)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c w") 'woman)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c q") 'query-replace)


(global-set-key (kbd "C-c f") 'recentf-open-files)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; delete-backward-char DEL
;; backward-kill-word M-DEL

;; delete-forward-char C-d
;; kill-word M-d

(define-key global-map [C-f8]            'first-error)
(define-key global-map [f8]              'next-error)
(define-key global-map [S-f8]            'previous-error)
(define-key global-map [f5]              'ispell-word)
(define-key global-map [f6]              'scroll-right)
(define-key global-map [f7]              'scroll-left)
(define-key global-map [f9]              'compile)
;; (define-key global-map [f10]             'start-kbd-macro)
;; (define-key global-map [f11]             'end-kbd-macro)
;; (define-key global-map [f12]             'call-last-kbd-macro)
(define-key global-map [kp-add]          'enlarge-window)
(define-key global-map [kp-subtract]     'shrink-window)
;; (define-key global-map [kp-decimal]      'what-cursor-position)
(global-set-key (kbd "<kp-decimal>") 'what-cursor-position)


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


(define-key global-map [M-down] '(lambda()
				   "scroll down, cursor remains in place"
				   (interactive)
				   (scroll-up 1)
				   (next-line 1)))

(define-key global-map [M-up] '(lambda()
				 "scroll up, cursor remains in place"
				 (interactive)
				 (scroll-up -1)
				 (next-line -1)))

(define-key global-map [mouse-5] '(lambda()
				    "scroll down one line"
				    (interactive)
				    (scroll-down -1)))

(define-key global-map [mouse-4] '(lambda()
				    "scroll up one line"
				    (interactive)
				    (scroll-down 1)))


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


;; (define-key global-map [M-f12] set_bidi_on())

;; (define-key global-map [M-f11] set_bidi_of())
