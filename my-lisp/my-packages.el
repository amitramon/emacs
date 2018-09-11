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

;; (package-initialize)
(defvar myPackages
  '(elpy
    magit
    dash))


(setq myPackages '(ein ;; add the ein package (Emacs ipython notebook)
		   py-autopep8 multi-web-mode web-mode
		   docker-compose-mode dockerfile-mode dash elpy yaml-mode
		   realgud markdown-mode magit dash-functional undo-tree lua-mode
		   fvwm-mode))

;; flychek -- unavailable?
;; "dash"
;; bookmark+

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)
