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

(defun my-spell ()
  "spell check a word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

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

