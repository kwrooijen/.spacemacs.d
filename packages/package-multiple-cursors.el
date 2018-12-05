(use-package multiple-cursors
  :bind* (("M-K" . mc/mark-previous-like-this)
          ("M-J" . mc/mark-next-like-this))
  :bind (:map mc/keymap
              ("<return>" . newline))
  :init
  :config
  (multiple-cursors-mode t)
  (define-key org-mode-map (kbd "M-J") 'mc/mark-next-like-this)
  (define-key org-mode-map (kbd "M-K") 'mc/mark-previous-like-this))

(provide 'package-multiple-cursors)
