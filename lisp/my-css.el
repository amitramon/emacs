
(defun key-comment (key &optional comment-it)
  "Return a commented string containing key."
  (concat "/* " key (if comment-it "-OFF" "-ON") " */"))

(defun set-comment-state (key &optional comment-it)
  "Comment or uncomment lines based on a key and the comment-it parameter.
Lines to handle are identified by a string of the form /* <key>-[ON|OFF] */,
and commented out if comment-it is true, uncommented otherwise."
  (setq old-key (key-comment key (not comment-it)))
  (setq new-key (key-comment key comment-it))
  (setq pos (point))
  (beginning-of-buffer)
  (while (search-forward old-key nil t)
    (replace-match "" nil t)
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (if comment-it

	  (comment-region beg (point))
	  (uncomment-region beg (point))))
    (end-of-line)
    (insert (concat "   " new-key))
    (indent-according-to-mode))
  (goto-char pos))
(defun css-dbg-off ()
  "set 1"
  (set-comment-state "DBG" t)
  (set-comment-state "PRD"))

(defun css-dbg-on ()
  "set 1"
  (set-comment-state "DBG")
  (set-comment-state "PRD" t))

(define-key global-map [f19]
  (lambda ()
    "Turn off debug state"
    (interactive)
    (css-dbg-off)))

(define-key global-map [S-f19]
  (lambda ()
    "Turn on debug state"
    (interactive)
    (css-dbg-on)))


(defun prev-window ()
  "go to the previous window"
  (interactive)
  (other-window -1))

(global-set-key "\C-cp" 'prev-window)
