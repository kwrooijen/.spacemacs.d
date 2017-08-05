(require 'lispyville)
(require 'lispy)

(add-hook 'lispy-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'clojure-mode-hook #'lispy-mode)

(evil-define-key 'insert lispy-mode-map
  "[" #'lispy-brackets-or-barf)

(evil-define-key 'insert lispy-mode-map
  (kbd "M-[") #'lispy-wrap-brackets-or-reverse-barf)

(defun lispy--mode-p ()
  (or (lispy-left-p)
      (lispy-right-p)))

(defun lispy-left-insert ()
  (interactive)
  (when (not (lispy--mode-p))
    (lispy-left 1))
  (when (not (lispy--mode-p))
    (beginning-of-defun))
  (evil-insert-state 1))

(defun lispy-beginning-of-defun-insert ()
  (interactive)
  (end-of-line)
  (beginning-of-defun)
  (evil-insert-state 1))

(defun lispy-global-teleport (arg)
  (interactive "p")
  (let ((lispy-teleport-global t))
    (lispy-teleport arg)))

(defun lispy-brackets-or-barf (arg)
  (interactive "P")
  (if (lispy--mode-p)
      (lispy-barf 1)
    (lispy-brackets arg)))

(defun lispy-wrap-brackets-or-reverse-barf ()
  (interactive)
  (if (lispy--mode-p)
      (save-excursion
        (lispy-different)
        (lispy-barf 1))
    (lispy-wrap-brackets)))

(defun lispy-reverse-slurp ()
  (interactive)
  (save-excursion
    (lispy-different)
    (lispy-slurp 1)))

(eval-after-load "lispy"
  `(progn
     (define-key lispyville-mode-map (kbd "M-w") 'lispyville-yank)
     (define-key lispy-mode-map (kbd "M-a") 'lispy-left-insert)
     (define-key lispy-mode-map (kbd "M-A") 'lispy-beginning-of-defun-insert)
     (define-key lispy-mode-map (kbd "C-S") 'lispy-unstringify)
     (define-key lispy-mode-map (kbd "M-i") 'tab-to-tab-stop)
     (lispy-define-key lispy-mode-map "M-]" 'lispy-reverse-slurp)
     (lispy-define-key lispy-mode-map "o" 'evil-open-below)
     (lispy-define-key lispy-mode-map "x" 'lispy-delete)
     (lispy-define-key lispy-mode-map "]" 'lispy-slurp)
     (lispy-define-key lispy-mode-map "A" 'lispy-ace-symbol-replace)
     (lispy-define-key lispy-mode-map "H" 'special-lispy-move-left)
     (lispy-define-key lispy-mode-map "J" 'special-lispy-down-slurp)
     (lispy-define-key lispy-mode-map "K" 'special-lispy-up-slurp)
     (lispy-define-key lispy-mode-map "L" 'special-lispy-move-right)
     (lispy-define-key lispy-mode-map "I" 'evil-insert-state)
     (lispy-define-key lispy-mode-map "T" 'lispy-global-teleport)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (lispy-define-key lispy-mode-map (kbd "e") 'cider-eval-sexp-at-point)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (lispy-define-key lispy-mode-map (kbd "e") 'lispy-eval)))

(defadvice lispy-bind-variable (after lispy-bind-variable activate)
  (evil-insert-state 1))

(provide 'package-lispy)
