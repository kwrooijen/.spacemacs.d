(defun clj-hide-namespace ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (hs-hide-block)))

(defun my-clojure-mode-hook ()
  (when (equal "(ns " (buffer-substring-no-properties 1 5))
    (clj-hide-namespace)))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'cider-mode-hook
          (lambda () (setq next-error-function #'flycheck-next-error-function)))

(add-hook 'clojure-mode-hook #'turn-off-smartparens-mode)
(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'clojure-mode-hook #'flycheck-mode)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "n" 'clj-hide-namespace
  "c" 'clojure-cheatsheet)

(provide 'package-clojure)
