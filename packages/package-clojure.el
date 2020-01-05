(require 'functions)
(require 'cider)
(require 'flycheck-joker)
(require 'flycheck-clj-kondo)
(require 'doom-modeline-segments)

(setq cider-auto-jump-to-error nil)

(defun clj-hide-namespace ()
  (interactive)
  (when (and (> (buffer-end 1) 5)
             (equal "(ns " (buffer-substring-no-properties 1 5)))
    (save-excursion
      (beginning-of-buffer)
      (hs-hide-block))))

(add-hook* 'clojurescript-mode-hook
           (subword-mode -1)
           (flycheck-mode)
           (setq cider-enhanced-cljs-completion-p nil
                 cider-default-cljs-repl 'shadow))

(add-hook* 'clojure-mode-hook
           (setq cider-auto-select-test-report-buffer nil)
           (subword-mode -1)
           (doom-modeline-set-modeline 'clojure)
           (clj-hide-namespace)
           (flycheck-mode))

(define-key clojure-mode-map (kbd "M-:") 'clojure-eval-expression)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "n" 'clj-hide-namespace
  "c" 'clojure-view-cheatsheet)

(defun clojure-eval-expression (input)
  (interactive "sEval: ")
  (cider-interactive-eval input))

(define-clojure-indent
  (render 1)
  (s/fdef 1)
  (dom/div 1)
  (let-if 1))

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
 (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(dolist (checkers '((clj-kondo-clj . clojure-joker)
                   (clj-kondo-cljs . clojurescript-joker)
                   (clj-kondo-cljc . clojure-joker)
                   (clj-kondo-edn . edn-joker)))
 (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

(add-hook 'clojure-mode-hook 'flycheck-mode)

(use-package clojure-mode
 :ensure t
 :config
 (require 'flycheck-clj-kondo))

;; (defun special-lispy-minibuffer-eval ()
;;   "For ClojurScript"
;;   (interactive)
;;   (cider-eval-sexp-at-point))

(provide 'package-clojure)
