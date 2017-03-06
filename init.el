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
     ranger
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
   dotspacemacs-additional-packages '(key-chord scss-mode uuidgen simpleclip lispy lispyville rainbow-mode)
   dotspacemacs-excluded-packages '(evil-escape linum multiple-cursors)
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
   dotspacemacs-themes '(spacemacs-dark spacemacs-light)
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

  (spaceline-toggle-minor-modes-off)
  (global-evil-mc-mode)
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
   ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-mode zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode simpleclip shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters racer quelpa pug-mode popwin persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-elixir neotree multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode lispyville linum-relative link-hint less-css-mode key-chord js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md geiser flycheck-rust flycheck-pos-tip flycheck-mix flycheck-elm flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode elm-mode elisp-slime-nav dumb-jump dockerfile-mode docker diff-hl define-word company-web company-tern company-statistics company-quickhelp column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cargo auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adaptive-wrap ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
