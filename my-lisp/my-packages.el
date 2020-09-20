(require 'package)

;; (add-to-list 'package-archives
;;        '("melpa" . "http://melpa.org/packages/") t)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))


(setq package-archive-priorities '(("melpa" . 10)
				   ("gnu" . 5)
				   ("org" . 2)
				   ;; ("marmalade" . 0)
				   ))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; the following code will install packages listed in myPackages if
;; they are not allready installed
;; https://realpython.com/emacs-the-best-python-editor/

(defvar my-packages-package-list "List of custom packages to install.")

;;; this allows for dynamically update and install packages while
;;; Emacs is running, by modifying this list, and then evaluating it
;;; and tha mapc expression below it
(setq my-packages-package-list
      '(;; add the ein package (Emacs ipython notebook)
	ein

	;; python development environment
	elpy

	;; beutify python code
	py-autopep8

	;; git emacs interface
	magit

	;; debuggers front end
	realgud

	;; multiple major mode for web editing
	;; multi-web-mode

	;; major mode for editing web templates
	web-mode

	;; docker modes
	docker-compose-mode
	dockerfile-mode

	;; list library for emacs
	dash
	;; collection of useful combinators for emacs lisp
	dash-functional

	;; major modes for yaml
	yaml-mode

	;; major modes for markdown
	markdown-mode

	;; major modes for lua
	lua-mode

	;; major modes for fvwm config files
	fvwm-mode

	;; treat undo history as a tree
	;; undo-tree

	;; flychek
	;; flychek-clojure
	;; flychek-pycheckers

	;; Clojure for the brave and true - below; amit - some packages
	;; commented out by me until I'll be sure they are needed
	
	;; makes handling lisp expressions much, much easier
	;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
	paredit

	;; key bindings and code colorization for Clojure
	;; https://github.com/clojure-emacs/clojure-mode
	clojure-mode

	;; extra syntax highlighting for clojure
	clojure-mode-extra-font-locking

	;; integration with a Clojure REPL
	;; https://github.com/clojure-emacs/cider
	;; cider

	;; allow ido usage in as many contexts as possible. see
	;; customizations/navigation.el line 23 for a description
	;; of ido
	;; ido-ubiquitous

	;; Enhances M-x to allow easier execution of commands. Provides
	;; a filterable list of possible commands in the minibuffer
	;; http://www.emacswiki.org/emacs/Smex
	;; smex

	;; project navigation
	;; projectile

	;; colorful parenthesis matching
	rainbow-delimiters

	;; solarized theme
	solarized-theme
	
	;; edit html tags like sexps
	;; tagedit

	;; help finding keys
	which-key

	;; xkcd
	xkcd

	;; Clojure exercises
	4clojure

	;; needed by org-mode to generate HTML
	htmlize

	;; R
	ess
	ess-R-data-view
	;; ess-smart-equal
	;; ess-smart-underscore
	ess-view
	polymode
	poly-markdown
	poly-R
	)
      )

;; bookmark+

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      my-packages-package-list )
