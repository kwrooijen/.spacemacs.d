(require 'functions)

(defun clj-hide-namespace ()
  (interactive)
  (when (equal "(ns " (buffer-substring-no-properties 1 5))
    (save-excursion
      (beginning-of-buffer)
      (hs-hide-block))))

(eval-after-load 'flycheck '(flycheck-clojure-setup))

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook* 'cider-mode-hook
           (setq next-error-function #'flycheck-next-error-function))

(add-hook 'clojure-mode-hook 'clj-hide-namespace)
(add-hook 'clojure-mode-hook #'turn-off-smartparens-mode)
(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'clojure-mode-hook #'flycheck-mode)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "n" 'clj-hide-namespace
  "c" 'clojure-cheatsheet)

;; ClojureScript

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(provide 'package-clojure)
