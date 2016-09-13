(use-package hlinum
  :ensure t
  :config
  (set-face-attribute 'linum-highlight-face nil
                      :inherit 'hl-line-face
                      :background nil
                      :foreground "DodgerBlue")
  (hlinum-activate))

(provide 'package-hlinum)
