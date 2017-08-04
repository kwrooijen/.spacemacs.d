;; Lispy
(require 'evil-lispy)
(require 'lispyville)
(add-hook 'evil-lispy-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
(add-hook 'clojure-mode-hook #'evil-lispy-mode)

(evil-define-key 'normal evil-lispy-mode-map
  "x" #'lispy-delete)

(evil-define-key 'insert evil-lispy-mode-map
  "[" #'lispy-brackets)

(define-key evil-lispy-mode-map (kbd "M-a") 'evil-lispy/enter-state-left)
(define-key evil-lispy-mode-map (kbd "C-S") 'lispy-unstringify)
(define-key lispyville-mode-map (kbd "M-w") 'lispyville-yank)
(define-key lispy-mode-map (kbd "i") 'lispy-tab)
(define-key lispy-mode-map (kbd "f") 'lispy-flow)
(define-key lispy-mode-map (kbd "d") 'special-lispy-different)
(define-key lispy-mode-map (kbd "o") 'special-lispy-other-mode)
(define-key lispy-mode-map (kbd "]") 'special-lispy-slurp)
(define-key lispy-mode-map (kbd "[") 'special-lispy-barf)
(define-key lispy-mode-map (kbd "A") 'evil-lispy-ace-symbol-replace)
(define-key lispy-mode-map (kbd "H") 'special-lispy-move-left)
(define-key lispy-mode-map (kbd "J") 'special-lispy-down-slurp)
(define-key lispy-mode-map (kbd "K") 'special-lispy-up-slurp)
(define-key lispy-mode-map (kbd "L") 'special-lispy-move-right)
(define-key lispy-mode-map (kbd "I") 'evil-insert-state)
(define-key lispy-mode-map (kbd "T") 'lispy-global-teleport)
(define-key lispy-mode-map (kbd "M-i") 'tab-to-tab-stop)

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key lispy-mode-map (kbd "e") 'cider-eval-sexp-at-point)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key lispy-mode-map (kbd "e") 'lispy-eval)))

(defadvice lispy-bind-variable (after lispy-bind-variable activate)
  (evil-insert-state 1))

(defun lispy-global-teleport (arg)
  (interactive "p")
  (let ((lispy-teleport-global t))
    (lispy-teleport arg)))

(defun evil-lispy-ace-symbol-replace (arg)
  "Jump to a symbol withing the current sexp and delete it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (lispy-ace-symbol arg)
  (when (region-active-p)
    (lispy-delete 1)
    (evil-insert-state)))

(provide 'package-lispy)
