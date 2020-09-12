;; Emacs init file
;; 

;; (defvar *my/emacs-dir* (file-name-directory user-init-file)
;;   "The directory containing Emacs' init file and other user files")

(defun my/update-path (dir)
  "Add dir to system path"
  (if (file-exists-p dir)
      (add-to-list 'exec-path dir)))


(defun my/rel-to-emacs-dir-path (relpath)
  "Create the full path of 'relpath' relative to the user's
Emacs' directory."
  (concat user-emacs-directory
	  (convert-standard-filename relpath)))

(add-to-list 'load-path (my/rel-to-emacs-dir-path "my-lisp"))

;;; When running Emacs as a systemd user service it doesn't pick the
;;; user's path, so we update it here
(my/update-path "~/scripts")
(my/update-path "~/bin")
(my/update-path "~/.local/bin")
(my/update-path "/usr/local/bin") ;; for mac

;; hack to allow connecting to elpa (bug in emacs)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;------------------------------------------------------------
; Load some libraries
;------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
(load "my-packages") ; ELPA management, calls (package-initialize)

;;; TMPFIX: call this after package-initialize to add load subdirs
;;; before elpa in order to load fixed version of dictionary.el
;;; (add-to-list 'load-path "~/src/dictionary-el")

;;;(load "dictionary")


;;------------------------------------------------------------
;; ido
;;------------------------------------------------------------
;; thanks to "Mastering Emacs" for some tips
;; https://masteringemacs.org/article/introduction-to-ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".org" ".txt" ".py"
				  ".el" ".clj" ".cljs"
				  ".xml" ".el" ".ini"
				  ".cfg" ".cnf"))
(setq ido-create-new-buffer 'always)
(ido-mode 1)

;------------------------------------------------------------
; keyboard & input methods
;------------------------------------------------------------
;;;>>> (load "quail-custom-keyboard-layouts") ; Custom layouts not installed with Emacs
;;;>>> (quail-set-keyboard-layout "us-dvorak")
(load "quail-dvorak")

(setq default-input-method "hebrew")
(setq-default bidi-display-reordering t)
(windmove-default-keybindings)

;------------------------------------------------------------
; Load my libraries
;------------------------------------------------------------
(load "my-utils")    ; general utilities
(load "my-keys")     ; key bindings & functions
(load "my-calendar") ; calendar customizations
(load "my-modes")    ; customizations to modes & hooks
(load "my-rst")	     ; reStructures text customizations
(load "my-dict")     ; dictionary customizations
(load "my-abbreviations")
;>> (load "my-server")
(load "my-org")	     ; org-mode settings
(load "my-desktop")  ; desktop-mode settings

;; Clojure for the brave and true - below
;; For editing lisps
(load "elisp-editing.el")
;; Langauage-specific
(load "setup-clojure.el")
(load "setup-R.el")
;; (load "setup-js.el")


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

;; (set-face-foreground 'highlight "white")
;; (set-face-background 'highlight "blue")
(set-face-attribute 'default nil :height 140)

;; settings for new frames, e.g. those open by emacsclient --- the
;; font setting override the height setting above. not sure I need it.
;; (add-to-list 'default-frame-alist '(font . "Lucida Sans
;; Typewriter-10"))

(load-theme 'solarized-dark t)
;; (load-theme 'wombat t)
;; (load-theme 'solarized-light t)

;;; display line number in all buffers
;;; use display-line-numbers-mode to toggle for specific buffer
(global-display-line-numbers-mode)

;------------------------------------------------------------
; Tools
;------------------------------------------------------------

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(setq compile-command "make -k ")


;------------------------------------------------------------
; Python configuration
;------------------------------------------------------------

(elpy-enable)				; python elpy

;; use python3 for python shell
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")

;; use jupyter 
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt" ; --kernel=python3
;;       python-shell-prompt-detect-failure-warning nil)

;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")


;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; flychek hely says:
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; test to verify this is necessary
(setq elpy-rpc-backend "jedi")



;------------------------------------------------------------
; Misc settings
;------------------------------------------------------------

(setq vc-follow-symlinks t)		; automatically follow symbolic links to files

;; recentf stuff
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)			; remember recent files
(setq recentf-max-menu-items 25)
;; (global-set-key "C-x C-r" 'recentf-open-files)

; set unicode data file location. (used by what-cursor-position)
; UnicodeData.txt is provided by Debian's package unicode-data
(let ((x "/usr/share/unicode/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))

(setq-default ispell-program-name "aspell")

(setq x-select-enable-clipboard t) ; enable pasting from the X clipboard
(setq x-select-enable-clipboard-manager nil) ; when t causes emacs to
					     ; hang on killing a frame

;;(setq scroll-step 4)
(setq colon-double-space t)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

; return a backup file path of a give file path
; with full  directory mirroring from a root dir
; non-existant dir will be created
(defun my/backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let (backup-root bpath)
    ;; (setq backup-root "~/var/backup/emacs/files-backup")
    (setq backup-root (my/rel-to-emacs-dir-path ".backup"))
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath
    ))

(setq make-backup-file-name-function 'my/backup-file-name)

(setq auto-save-list-file-prefix (my/rel-to-emacs-dir-path ".auto-save-list/saves-"))

(setq woman-use-own-frame nil)

(setq custom-file (my/rel-to-emacs-dir-path "custom-settings.el"))
(load custom-file)

(put 'dired-find-alternate-file 'disabled nil)

;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; "always" means no asking
(setq dired-recursive-deletes (quote top)) ; "top" means ask once
;; default target other dired buffer
(setq dired-dwim-target t)

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
(put 'erase-buffer 'disabled nil)
