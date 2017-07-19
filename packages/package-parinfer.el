(require 'parinfer)
(require 'parinfer-ext)
(require 'parinferlib)

(setq parinfer-extensions
      '(defaults pretty-parens evil
         lispy paredit smart-tab smart-yank))


(add-hook 'emacs-lisp-mode-hook #'turn-off-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)

(add-hook 'yas-before-expand-snippet-hook
          (lambda ()
            (when (member major-mode '(clojure-mode emacs-lisp-mode))
              (parinfer-mode -1))))

(add-hook 'yas-after-exit-snippet-hook
          (lambda ()
            (when (member major-mode '(clojure-mode emacs-lisp-mode))
              (parinfer-mode))))

(defun parinfer-lispy:backward-insert ()
  (interactive)
  (parinfer-lispy:backward)
  (evil-insert 0))

(parinfer-strategy-add 'instantly
  '(parinfer-smart-tab:dwim-right))

(defadvice evilnc-comment-operator (after evilnc-comment-operator activate)
  (if parinfer-mode
      (progn
        (beginning-of-line)
        (parinfer--invoke-parinfer-instantly)
        (back-to-indentation))))

(defadvice parinfer-smart-tab:dwim-right (before parinfer-smart-tab:dwim-right-before activate)
  (if (region-active-p)
      (progn
        (evil-visual-char)
        (if (< (point) (region-end)) (exchange-point-and-mark))
        (backward-char 1)
        (beginning-of-line))))

(defadvice parinfer-smart-tab:dwim-left (before parinfer-smart-tab:dwim-left-before activate)
  (if (region-active-p)
      (progn
        (evil-visual-char)
        (if (< (point) (region-end)) (exchange-point-and-mark))
        (backward-char 1)
        (beginning-of-line))))

(defadvice parinfer-smart-tab:dwim-right (after parinfer-smart-tab:dwim-right-after activate)
  (back-to-indentation))

(defadvice parinfer-smart-tab:dwim-left (after parinfer-smart-tab:dwim-left-after activate)
  (back-to-indentation))

(defun my-parinfer-toggle-mode ()
  "docstring"
  (interactive)
  (if parinfer-mode
      (parinfer-toggle-mode)
    (parinfer-mode)))

(defun my-parinfer-mode-hook ()
  (define-key parinfer-mode-map (kbd "M-]") 'parinfer--reindent-sexp)
  (define-key parinfer-mode-map (kbd "M-m") 'back-to-indentation)
  (define-key parinfer-mode-map (kbd "M-a") 'parinfer-lispy:backward-insert)
  (define-key parinfer-mode-map (kbd "M-{") 'paredit-backward-slurp-sexp)
  (define-key parinfer-mode-map (kbd "M-}") 'paredit-forward-slurp-sexp)
  (define-key parinfer-mode-map (kbd "M-j") 'paredit-join-sexps)
  (define-key parinfer-mode-map (kbd "<backtab>") 'parinfer-smart-tab:dwim-left)
  (define-key parinfer-mode-map (kbd "<tab>") 'parinfer-smart-tab:dwim-right)
  (define-key parinfer-region-mode-map (kbd "<backtab>") 'parinfer-smart-tab:dwim-left)
  (define-key parinfer-region-mode-map (kbd "<tab>") 'parinfer-smart-tab:dwim-right))


(require 'clojure-mode)
(define-key clojure-mode-map (kbd "M-q") 'my-parinfer-toggle-mode)
(define-key emacs-lisp-mode-map (kbd "M-q") 'my-parinfer-toggle-mode)

(add-hook 'parinfer-mode-hook 'my-parinfer-mode-hook)

(provide 'package-parinfer)
