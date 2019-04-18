(require 'functions)
(require 'cider)

(setq cider-auto-jump-to-error nil
      cider-lein-parameters "trampoline repl :headless :host ::")

(defun clj-hide-namespace ()
  (interactive)
  (when (and (> (buffer-end 1) 5)
             (equal "(ns " (buffer-substring-no-properties 1 5)))
    (save-excursion
      (beginning-of-buffer)
      (hs-hide-block))))

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))

;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; (add-hook* 'cider-mode-hook
;;            (setq next-error-function #'flycheck-next-error-function))

(add-hook* 'clojure-mode-hook
           (clj-hide-namespace)
           (turn-off-smartparens-mode)
           (flycheck-mode)
           (rainbow-delimiters-mode-disable)
           (global-highlight-parentheses-mode -1))

(define-key clojure-mode-map (kbd "M-:") 'clojure-eval-expression)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "n" 'clj-hide-namespace
  "c" 'clojure-cheatsheet)

(defun clojure-eval-expression (input)
  (interactive "sEval: ")
  (cider-interactive-eval input))

;; ClojureScript
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl)
           (user/cljs-repl))")

(define-clojure-indent
  (render 1)
  (s/fdef 1)
  (dom/div 1))

(defface paren-face
  '((((class color) (background dark))
     (:foreground "grey20"))
    (((class color) (background light))
     (:foreground "grey80")))
  "Face used to dim parentheses.")

(add-hook 'clojure-mode-hook
          (lambda ()
            (font-lock-add-keywords nil '((")" . 'paren-face)))))

(provide 'package-clojure)
