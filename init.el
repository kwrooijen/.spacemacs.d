(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(html
     dash
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     elixir
     elm
     lua
     javascript
     emacs-lisp
     erc
     clojure
     git
     docker
     syntax-checking
     yaml
     git
     markdown
     org
     racket
     rust
     scheme
     ;; spacemacs-layouts
     (shell :variables
            shell-default-height 20
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-protect-eshell-prompt t)
     ;; spell-checking
     version-control)
   dotspacemacs-additional-packages '(solaire-mode all-the-icons key-chord scss-mode uuidgen simpleclip parinfer lispy lispyville rainbow-mode powerline string-inflection)
   dotspacemacs-excluded-packages '(evil-escape linum multiple-cursors spaceline )
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes '(doom-one spacemacs-dark spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Mono"
                               :size 13
                               :weight bold
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "C-M-]"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key nil
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'all
   dotspacemacs-helm-use-fuzzy 'source
   ))

(defun dotspacemacs/user-init ()
  (add-to-load-path "~/.spacemacs.d/packages")
  (add-to-load-path "~/.spacemacs.d"))

(defun dotspacemacs/user-config ()
  (require 'functions)
  (require 'keys)

  (load-my-packages)

  (add-hook* 'prog-mode-hook (key-chord-mode 1))
  (add-hook* 'isearch-mode-hook (key-chord-mode 1))

  (setq
   alchemist-test--mode-name-face nil
   anzu-cons-mode-line-p nil
   anzu-mode-line-update-function 'spacemacs/anzu-update-mode-line
   diff-hl-side 'left
   elm-format-on-save t
   flycheck-pos-tip-timeout 9999
   git-gutter-fr+-side 'left-fringe
   global-hl-line-mode t
   global-page-break-lines-mode t
   helm-dash-browser-func 'eww-other-window
   helm-display-header-line nil
   helm-echo-input-in-header-line t
   helm-make-comint t
   helm-make-named-buffer t
   indent-buffer-modes '(scss-mode)
   neo-auto-indent-point nil
   neo-dont-be-alone nil
   neo-modern-sidebar nil
   neo-persist-show nil
   neo-show-hidden-files nil
   neo-smart-open nil
   projectile-enable-caching t
   projectile-use-git-grep t
   scroll-error-top-bottom t
   spaceline-helm-mode t
   spaceline-info-mode t
   x-select-enable-clipboard nil
   )

  ;; Parinfer

  (require 'parinfer)
  (require 'parinfer-ext)
  (require 'parinferlib)

  (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
  (add-hook 'clojure-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'clojure-mode-hook #'parinfer-mode)

  (define-key parinfer-mode-map (kbd "M-q") 'parinfer-toggle-mode)
  (define-key parinfer-mode-map (kbd "<tab>") 'parinfer-smart-tab:dwim-right)
  (define-key parinfer-region-mode-map (kbd "<tab>") 'parinfer-smart-tab:dwim-right)

  (parinfer-strategy-add 'instantly
    '(parinfer-smart-tab:dwim-right))

  (defadvice evilnc-comment-operator (after evilnc-comment-operator activate)
    (if parinfer-mode
        (progn
          (beginning-of-line)
          (parinfer--invoke-parinfer)
          (back-to-indentation))))

  (defadvice parinfer-smart-tab:dwim-right (before parinfer-smart-tab:dwim-right activate)
    (if (region-active-p)
        (progn
          (evil-visual-char)
          (if (< (point) (region-end)) (exchange-point-and-mark))
          (backward-char 1)
          (beginning-of-line))))

  ;; (add-hook 'clojure-mode-hook #'lispyville-mode)
  ;; (add-hook 'clojure-mode-hook #'paredit-mode)
  ;; (add-hook 'clojure-mode-hook #'lispy-mode)
  ;; (add-hook 'lispy-mode-hook #'lispyville-mode)
  ;; (lispy-set-key-theme '(lispy c-digits additional))
  ;; (define-key 'lispyville-mode-map (kbd "M-j"))
  (define-key global-map (kbd "C-x 1") 'delete-other-windows)

  (load-file "~/.spacemacs.d/modeline.el")

  ;; Fix dired buffer issues
  (setq persp-set-frame-buffer-predicate nil)
  (setq persp-when-kill-switch-to-buffer-in-perspective nil)
  (setq persp-add-buffer-on-after-change-major-mode t)

  ;; Random things to hopefully prevent crashing
  (setq dotspacemacs-mode-line-unicode-symbols nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-default))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   [("#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF")])
 '(cider-auto-jump-to-error nil)
 '(custom-safe-themes
   (quote
    ("6b1e6953a08acf12843973ec25d69dbfa1a53d869f649dc991a56fbdf0d7eb9e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ead76c417365064889c6552678e62a3982f9c6b359888dd7b2ba62efb9422b96" default)))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#5B6268" t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (parinfer auto-auto-indent flycheck-clojure clojure-cheatsheet helm-clojuredocs auto-indent-mode evil-lispy solaire-mode string-inflection racket-mode faceup orgit sublimity mini-header-line dired+ vline winum fuzzy flycheck-credo evil-multiedit doom-themes powerline alert log4e gntp org markdown-mode skewer-mode simple-httpd lispy zoutline swiper ivy js2-mode parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flycheck flx magit git-commit with-editor smartparens iedit anzu evil goto-chg f json-mode tablist magit-popup docker-tramp json-snatcher json-reformat diminish web-completion-data dash-functional tern pos-tip hydra inflections edn multiple-cursors paredit s peg eval-sexp-fu highlight cider seq spinner queue clojure-mode rust-mode bind-map bind-key yasnippet packed company dash elixir-mode pkg-info epl ace-window helm avy helm-core async auto-complete popup package-build rainbow-mode zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode simpleclip shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters racer quelpa pug-mode popwin persp-mode pcre2el paradox org-projectile org-present org-pomodoro org-download org-bullets open-junk-file ob-elixir neotree multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode lispyville linum-relative link-hint less-css-mode key-chord js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md geiser flycheck-rust flycheck-pos-tip flycheck-mix flycheck-elm flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode elm-mode elisp-slime-nav dumb-jump dockerfile-mode docker diff-hl define-word company-web company-tern company-statistics company-quickhelp column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cargo auto-yasnippet auto-highlight-symbol auto-compile alchemist  adaptive-wrap ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(vc-annotate-background "#1B2229")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
