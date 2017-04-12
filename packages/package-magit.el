(use-package magit
    :config
    (add-hook* 'magit-mode-hook (setq cursor-in-non-selected-windows nil))
    (define-key magit-mode-map "\M-1" 'winum-select-window-1)
    (define-key magit-mode-map "\M-2" 'winum-select-window-2)
    (define-key magit-mode-map "\M-3" 'winum-select-window-3)
    (define-key magit-mode-map "\M-4" 'winum-select-window-4)
    (define-key magit-mode-map "\M-5" 'winum-select-window-5)
    (define-key magit-mode-map "\M-6" 'winum-select-window-6)
    (define-key magit-mode-map "\M-7" 'winum-select-window-7)
    (define-key magit-mode-map "\M-8" 'winum-select-window-8)
    (define-key magit-mode-map "\M-9" 'winum-select-window-9))

(provide 'package-magit)
