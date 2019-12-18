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

(defun cider-last-test-succeededp ()
  (zerop
   (+ (lax-plist-get (cdr cider-test-last-summary) "error")
      (lax-plist-get (cdr cider-test-last-summary) "fail"))))

(doom-modeline-def-segment cider-test-error
  (doom-modeline--modal-icon "Test status"
                             (if (cider-last-test-succeededp)
                                 'compilation-info
                               'compilation-error)
                             "Test status"))

(doom-modeline-def-modeline 'clojure
  '(bar workspace-name window-number cider-test-error matches buffer-info remote-host buffer-position word-count parrot selection-info)
  '(objed-state misc-info persp-name battery grip irc mu4e github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

(add-hook* 'clojure-mode-hook
           (add-hook 'after-save-hook
                     (lambda ()
                       (when (and (cider-connected-p)
                                  (equal major-mode 'clojure-mode))
                         (cider-test-run-project-tests nil))) nil 'local)
           (setq cider-auto-select-test-report-buffer nil)
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

(provide 'package-clojure)
