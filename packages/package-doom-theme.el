(use-package doom-themes
  :init
  (require 'f)
  (load-file "~/.spacemacs.d/modeline.el")
  (add-hook 'find-file-hook 'doom-buffer-mode)
  (load-theme 'doom-one)
  (require 'doom-neotree)
  (setq doom-neotree-enable-folder-icons t)
  (setq doom-neotree-enable-file-icons t)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-attribute 'mode-line nil :box nil))

(provide 'package-doom-theme)
