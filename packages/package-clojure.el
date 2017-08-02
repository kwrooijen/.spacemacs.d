(require 'functions)
(require 'cider)

(setq cider-auto-jump-to-error nil
      cider-boot-parameters "trampoline repl -s -H :: wait")

(defun clj-hide-namespace ()
  (interactive)
  (when (and (> (buffer-end 1) 5)
             (equal "(ns " (buffer-substring-no-properties 1 5)))
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
(add-hook 'clojure-mode-hook #'flycheck-mode)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "n" 'clj-hide-namespace
  "c" 'clojure-cheatsheet
  "\"" 'cider-figwheel-repl)

;; ClojureScript

(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))


(provide 'package-clojure)
