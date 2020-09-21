

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

