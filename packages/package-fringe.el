  (use-package fringe
    :config
    (setq-default fringes-outside-margins t)
    (set-fringe-mode '1)
    (add-hook 'prog-mode-hook 'linum-mode)
    (advice-add 'neo-global--select-window :after (lambda ()
                                                    (setq cursor-in-non-selected-windows nil)
                                                    (set-window-fringes neo-global--window 1 0)
                                                    (spacemacs/toggle-mode-line-off)))

    (defun small-fringe ()
      (if (not (active-minibuffer-window))
          (set-window-fringes (selected-window) 2 0 t)))

    (add-hook 'buffer-list-update-hook 'small-fringe)
    (set-face-foreground 'git-gutter-fr+-added "green")
    (set-face-background 'git-gutter-fr+-added "green")
    (set-face-foreground 'git-gutter-fr+-modified "yellow")
    (set-face-background 'git-gutter-fr+-modified "yellow")
    (set-face-foreground 'git-gutter-fr+-deleted "red")
    (set-face-background 'git-gutter-fr+-deleted "red")
    (set-face-background 'fringe "#262c34"))

(provide 'package-fringe)
