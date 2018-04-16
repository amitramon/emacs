
;;-----------------------
;; Diary & Calendar
;;-----------------------

;; 32.2853,34.9467
(setq calendar-latitude 32.2853)
(setq calendar-longitude 34.9467)
(setq calendar-location-name "Raanana, IL")
(setq calendar-time-zone +120)
(setq calendar-standard-time-zone-name "IST")
(setq calendar-daylight-time-zone-name "IDT")

;; Fix foolish calendar-mode scrolling,
;; plus some customizations.
(add-hook 'calendar-load-hook
	  '(lambda ()
	     ;;(setq mark-holidays-in-calendar t)
	     ;;(define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)
	     ;; (set-face-foreground 'diary-face   "magenta") ;; troubles with emacs 25?
	     (set-face-foreground 'calendar-today-face "white")
	     (set-face-background 'calendar-today-face "green")
	     ;;(set-face-background 'holiday-face "slate blue")
	     ;;(set-face-foreground 'holiday-face "white")
	     ))


;; Set calendar highlighting colors

(require 'calendar)
(calendar-set-date-style "european")

(setq calendar-view-diary-entries-initially-flag t
      calendar-mark-diary-entries-in-calendar-flag t
      number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)


;; launch calendar on startup
;;(calendar)

;; Here is some code to get rid of the ugly equal signs under the date:
(add-hook 'fancy-diary-display-mode-hook
	  '(lambda ()
	     (alt-clean-equal-signs)))
  
(defun alt-clean-equal-signs ()
  "This function makes lines of = signs invisible."
  (goto-char (point-min))
  (let ((state buffer-read-only))
    (when state (setq buffer-read-only nil))
    (while (not (eobp))
      (search-forward-regexp "^=+$" nil 'move)
      (add-text-properties (match-beginning 0) 
			   (match-end 0) 
			   '(invisible t)))
    (when state (setq buffer-read-only t))))


(define-derived-mode fancy-diary-display-mode  fundamental-mode
  "Diary"
  "Major mode used while displaying diary entries using Fancy Display."
  (set (make-local-variable 'font-lock-defaults)
       '(fancy-diary-font-lock-keywords t))
  (define-key (current-local-map) "q" 'quit-window)
  (define-key (current-local-map) "h" 'calendar))

(defadvice fancy-diary-display (after set-mode activate)
  "Set the mode of the buffer *Fancy Diary Entries* to
 `fancy-diary-display-mode'."
  (save-excursion
    (set-buffer fancy-diary-buffer)
    (fancy-diary-display-mode)))
