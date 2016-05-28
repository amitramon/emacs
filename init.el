;; Emacs init file
;; 

(setq load-path (cons "~/.emacs.d/lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp" load-path))
(setq load-path (cons "/usr/share/emacs/site-lisp/emacs-goodies-el" load-path))
(setq load-path (cons "/usr/share/emacs/site-lisp/dictionary-el" load-path))
(load "dictionary-init")

;------------------------------------------------------------
; Load some libraries
;------------------------------------------------------------
(load-library "muttrc-mode")
(load-library "apache-mode")
(load-library "quail-custom-keyboard-layouts") ; Custom layouts not installed with Emacs
(quail-set-keyboard-layout "us-dvorak")	       ; Set current keyboard layout

(load-library "hebrew-custom")			; Hebrew input methods
;; (load-library "hebrew-scot")			; Hebrew input
;; 						; methods, made by
;; 						; Yair F., sent to me
;; 						; by Scot.

(setq default-input-method "hebrew-custom-lyx")

(setq-default bidi-display-reordering t)

;; (load-library "hebrew-en-dvorak")			; Hebrew input methods
;; (setq default-input-method "hebrew-lyx-en-dvorak")

;------------------------------------------------------------
; Load my libraries
;------------------------------------------------------------
(load-library "my-utils")    ; general utilities
(load-library "my-keys")     ; key bindings & functions
(load-library "my-calendar") ; calendar customizations
(load-library "my-modes")    ; customizations to modes & hooks
(load-library "my-rst")	     ; reStructures text customizations
(load-library "my-dict")     ; dictionary customizations
(load-library "my-abbreviations")
;>> (load-library "my-server")
(load-library "my-org")	     ; org-mode settings
(load-library "my-desktop")  ; desktop-mode settings

;------------------------------------------------------------
; UI & Fonts
;------------------------------------------------------------
(blink-cursor-mode -1)			; -1 non-blinking, 1 blinks
(setq x-stretch-cursor t)

; Also set in .Xresources - here for terminals.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-font-lock-mode t)
(setq transient-mark-mode t)
(show-paren-mode t)

(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")

;------------------------------------------------------------
; Tools
;------------------------------------------------------------

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;------------------------------------------------------------
; Misc settings
;------------------------------------------------------------

;; (set-keyboard-coding-system 'utf-8)	; required, for some reason, for old emacs

(setq vc-follow-symlinks t)		; automatically follow symbolic links to files
					; under version-control

;; recentf stuff
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)			; remember recent files
(setq recentf-max-menu-items 25)
;; (global-set-key "C-x C-r" 'recentf-open-files)

; set unicode data file location. (used by what-cursor-position)
(let ((x "~/data/unicode/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))

(setq-default ispell-program-name "aspell")

(setq compile-command "r2w ~/new-docs/site.ini -w")

; enable pasting from the X clipboard
(setq x-select-enable-clipboard t)
;;(setq scroll-step 4)
(setq colon-double-space t)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(when (> emacs-major-version 23)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives 
	       '("melpa" . "http://melpa.milkbox.net/packages/")
	       'APPEND))

;; (setq Info-default-directory-list
;;       (append '("/usr/local/info/"
;; 		"/usr/local/share/info/"
;; 		"/usr/local/gnu/info/"
;; 		"/usr/local/gnu/lib/info/"
;; 		"/usr/local/gnu/lib/emacs/info/"
;; 		"/usr/local/emacs/info/"
;; 		"/usr/local/lib/info/"
;; 		"/usr/local/lib/emacs/info/"
;; 		"/usr/share/info/"
;; 		"/usr/local/texlive/2008/texmf/doc/info")
;; 	      Info-default-directory-list))


;; To add info files, add them to the `dir` file in the
;; /usr/local/info directory.
;; Note: after all, it seems it was enough to create a
;; proper /usr/local/info/dir file. Info probably looks
;; for it by default.

;; (setq Info-default-directory-list
;;       (append '("/usr/local/info/")
;; 	      Info-default-directory-list))


;; This seems not to work:

;; (require 'info)
;; (setq Info-directory-list
;;       (cons (expand-file-name "/usr/local/info")
;;             Info-directory-list))



; return a backup file path of a give file path
; with full directory mirroring from a root dir
; non-existant dir will be created
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let (backup-root bpath)
    (setq backup-root "~/var/backup/emacs/files-backup")
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)
(setq auto-save-list-file-prefix "~/var/backup/emacs/auto-save-list/.saves-")
(setq woman-use-own-frame nil)
;; settings for new frames, e.g. those open by emacsclient
(add-to-list 'default-frame-alist '(font . "Lucida Sans Typewriter-10"))

(setq x-select-enable-clipboard-manager nil) ; when t causes emacs to
					     ; hang on killing a frame

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(find-ls-option (quote ("-exec ls -ldh {} +" . "-ldh")))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(put 'dired-find-alternate-file 'disabled nil)

;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; "always" means no asking
(setq dired-recursive-deletes (quote top)) ; "top" means ask once
;; default target other dired buffer
(setq dired-dwim-target t)
