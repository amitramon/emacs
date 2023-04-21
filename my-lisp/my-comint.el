

(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [down]
       'comint-next-matching-input-from-input)
     ;; also recommended for ESS use --
     (setq comint-move-point-for-output 'others)
     ;; somewhat extreme, almost disabling writing in *R*, *shell*
     ;; buffers above prompt:
     (setq comint-scroll-to-bottom-on-input 'this)))


;; https://github.com/emacs-ess/ESS/issues/1199
;; Fuco1 workaround
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter)
  (smartparens-mode 1))

(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)



;; (use-package xterm-color
;;   :load-path "PATH-TO-xterm-color"

;;   :init
;;   (setq comint-output-filter-functions
;;         (remove 'ansi-color-process-output comint-output-filter-functions))

;;   (add-hook 'inferior-ess-mode-hook
;;             (lambda () (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t)))

;;   :config
;;   (setq xterm-color-use-bold t))
