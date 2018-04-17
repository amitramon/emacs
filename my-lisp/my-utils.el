; Thanks to brool, http://www.brool.com/
(defun view-encrypted-file (fname)
  "Open and decrypt a bcrypt-encrypted file."
  (interactive "FFind file: ")
  (let ((buf (create-file-buffer fname)))
    (shell-command
     (concat "echo " (read-passwd "Decrypt password: ") "| bcrypt -o " fname)
     buf)
    ;(set-buffer buf)
    (switch-to-buffer buf)
    (goto-char 0) ; ar
    (kill-line)(kill-line)
    (toggle-read-only)
    (not-modified))
  )

(defun edit-encrypted-file (fname)
  "Open and decrypt a bcrypt-encrypted file."
  (interactive "FFind file: ")
  (let ((buf (create-file-buffer fname)))
    (shell-command
     (concat "echo " (read-passwd "Decrypt password: ") "| bcrypt -o " fname)
     buf)
    ;(set-buffer buf)
    (switch-to-buffer buf)
    (goto-char 0) ; ar
    (kill-line)(kill-line)
    (toggle-read-only)
    (not-modified))
  )

;; (defun my-spell ()
;;   "spell check a word."
;;   (interactive)
;;   (flyspell-goto-next-error)
;;   (ispell-word))

;; emacswiki

(defun reverse-words (beg end)
  "Reverse the order of words in region."
  (interactive "*r")
  (apply
   'insert
   (reverse
    (split-string
     (delete-and-extract-region beg end) "\\b"))))

;; Based on Xah Lee's code.
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
;; Note: may want to try run-mailcap instead of xdg-open
(defun dired-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let (doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?")))
    (when doIt
      (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) myFileList))))



;;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-c SPC") 'push-mark-no-activate)
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

