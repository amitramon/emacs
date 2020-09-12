;; (setq display-buffer-alist
;;       `(("*R Dired"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . -1)
;;          (window-width . 0.33)
;;          (reusable-frames . nil))
o;;         ("*R"
;;          (display-buffer-reuse-window display-buffer-at-bottom)
;;          (window-width . 0.35)
;;          (reusable-frames . nil))
;;         ("*Help"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . 1)
;;          (window-width . 0.33)
;;          (reusable-frames . nil))))


(setq display-buffer-alist nil)

(require 'poly-markdown)
(require 'poly-R)

;; MARKDOWN

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;; R modes

(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(setq ess-style 'RStudio)

(setq tags-table-list
      '("~/dev/localize/localizebase" "~/dev/localize/localizebasesetup"))

;; (setq tags-file-name nil)
