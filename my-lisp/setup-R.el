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

(require 'poly-markdown)
(require 'poly-R)

;; MARKDOWN

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;; R modes

(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(setq ess-style 'RStudio)

;; (setq tags-table-list
;;       '("~/dev/localize/localizebase" "~/dev/localize/localizebasesetup"))

;; (setq tags-table-list nil)

;; (setq tags-file-name nil)




(defun ess-r-mode-hook-function ()
  ;; change original binding for ess-complete-object-name as it
  ;; shadows xref-find-references
  (define-key ess-r-mode-map (kbd "C-c c") 'ess-complete-object-name)
  (define-key ess-r-mode-map (kbd "M-?") nil)
  (xref-etags-mode)
  (setq tags-file-name "~/dev/localize/TAGS")
  )


(add-hook 'ess-r-mode-hook 'ess-r-mode-hook-function)

