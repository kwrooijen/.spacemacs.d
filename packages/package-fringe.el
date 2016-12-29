  (use-package fringe
    :config
    (set-face-foreground 'git-gutter+-added "green")
    (set-face-foreground 'git-gutter+-modified "yellow")
    (set-face-foreground 'git-gutter+-deleted "red")

    (set-face-background 'git-gutter+-added nil)
    (set-face-background 'git-gutter+-modified nil)
    (set-face-background 'git-gutter+-deleted nil))

(provide 'package-fringe)
