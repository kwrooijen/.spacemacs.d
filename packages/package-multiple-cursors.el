(use-package multiple-cursors
    :load-path "~/.spacemacs.d/multiple-cursors.el/"
    :bind* (("M-K" . mc/mark-previous-like-this)
            ("M-J" . mc/mark-next-like-this))
    :bind (:map mc/keymap
                ("<return>" . newline))
    :init
    (ensure-clone "myrjola" "multiple-cursors.el" "evil-compat")
    :config
    (multiple-cursors-mode t))

(provide 'package-multiple-cursors)
