(use-package magit
    :config
    (add-hook* 'magit-mode-hook (setq cursor-in-non-selected-windows nil))
    (define-key magit-mode-map "\M-1" 'select-window-1)
    (define-key magit-mode-map "\M-2" 'select-window-2)
    (define-key magit-mode-map "\M-3" 'select-window-3)
    (define-key magit-mode-map "\M-4" 'select-window-4)
    (define-key magit-mode-map "\M-5" 'select-window-5)
    (define-key magit-mode-map "\M-6" 'select-window-6)
    (define-key magit-mode-map "\M-7" 'select-window-7)
    (define-key magit-mode-map "\M-8" 'select-window-8)
    (define-key magit-mode-map "\M-9" 'select-window-9))

(provide 'package-magit)
