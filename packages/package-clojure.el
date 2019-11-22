(require 'functions)
(require 'cider)

(setq cider-auto-jump-to-error nil
      ;; cider-lein-parameters "trampoline repl :headless :host ::"
      )

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
           ;; (add-hook 'before-save-hook
           ;;           (lambda ()
           ;;             (when (cider-connected-p)
           ;;               (cider-format-buffer))) nil 'local)
           (clj-hide-namespace)
           (flycheck-mode))

(define-key clojure-mode-map (kbd "M-:") 'clojure-eval-expression)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "n" 'clj-hide-namespace
  "c" 'clojure-view-cheatsheet)

(defun clojure-eval-expression (input)
  (interactive "sEval: ")
  (cider-interactive-eval input))

;; ClojureScript
;; (setq cider-cljs-lein-repl
;;       "(do (require 'figwheel-sidecar.repl-api)
;;            (figwheel-sidecar.repl-api/start-figwheel!)
;;            (figwheel-sidecar.repl-api/cljs-repl)
;;            (user/cljs-repl))")

(define-clojure-indent
  (render 1)
  (s/fdef 1)
  (dom/div 1)
  (let-if 1))

(provide 'package-clojure)
