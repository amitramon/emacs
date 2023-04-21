;;; package --- Summary"

;;; Commentary:

;; (setq display-buffer-alist
;;       `(("*R Dired"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . -1)
;;          (window-width . 0.33)
;;          (reusable-frames . nil))
;;         ("*R"
;;          (display-buffer-reuse-window display-buffer-at-bottom)
;;          (window-width . 0.35)
;;          (reusable-frames . nil))
;;         ("*Help"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . 1)
;;          (window-width . 0.33)
;;          (reusable-frames . nil))))


;; (setq display-buffer-alist nil)

;;; Code:



(with-eval-after-load 'ess-r-mode
  (require 'ess-smart-equals)
  (ess-smart-equals-activate))


(defun ess-r-mode-hook-function ()
  "Ess hook."
  (xref-etags-mode)
  (setq tags-file-name "~/dev/localize/TAGS"))

(add-hook 'ess-r-mode-hook 'ess-r-mode-hook-function)

(setq ess-use-flymake nil)
(require 'flycheck)
(global-flycheck-mode t)

(provide 'setup-R)
;;; setup-R.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
