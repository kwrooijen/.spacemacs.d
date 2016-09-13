  (use-package doom-theme
    :load-path "~/.spacemacs.d/emacs-doom-theme/"
    :init
    (require 'f)
    (ensure-clone "hlissner" "emacs-doom-theme" "master")
    (load-file "~/.spacemacs.d/modeline.el")
    (require 'doom-themes)
    (add-hook 'find-file-hook 'doom-buffer-mode)
    (load-theme 'doom-one)
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-attribute 'mode-line nil :box nil))

(provide 'package-doom-theme)
