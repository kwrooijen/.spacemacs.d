(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     auto-completion
     (dash :variables
           dash-helm-dash-docset-path "~/.docsets")
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     elixir
     elm
     emacs-lisp
     erc
     clojure
     dockerfile
     git
     syntax-checking
     yaml
     git
     markdown
     org
     ranger
     rust
     scheme
     (shell :variables
            shell-default-height 20
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-protect-eshell-prompt t)
     ;; spell-checking
     version-control)
   dotspacemacs-additional-packages '(key-chord scss-mode twittering-mode uuidgen hlinum simpleclip doom-themes)
   dotspacemacs-excluded-packages '(vi-tilde-fringe)
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
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Mono"
                               :size 11
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
   dotspacemacs-line-numbers t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'all))

(defun dotspacemacs/user-init ()
  (add-to-load-path "~/.spacemacs.d/packages"))

(defun dotspacemacs/user-config ()

  (defmacro add-hook* (mode fn)
    `(add-hook ,mode (lambda () ,fn)))

  (defmacro ensure-clone (user project branch)
    (unless (file-exists-p (format "~/.spacemacs.d/%s" project))
      (shell-command (format "cd ~/.spacemacs.d && git clone -b %s git@github.com:%s/%s" branch user project))))

  (defun capitalize-previous-word ()
    (interactive)
    (save-excursion
      (backward-word)
      (capitalize-word 1)))

  (defun no-split ()
    (interactive)
    (setq-local split-width-threshold 2000)
    (setq-local split-height-threshold 2000))

  (defun eww-other-window (url)
    (interactive)
    (view-buffer-other-window "*DocBuffer*")
    (eww url))

  (defun evil-normal-state-and-save ()
    (interactive)
    (evil-normal-state)
    (save-buffer))

  (defun indent-buffer-on-save ()
    (if (member major-mode indent-buffer-modes)
        (spacemacs/indent-region-or-buffer)))

  (defun my/helm-exit-minibuffer ()
    (interactive)
    (helm-exit-minibuffer))

  (defun require-template (symbol)
    `(require (quote ,symbol)))

  (defmacro load-my-packages ()
    (let* ((filter-fn (lambda (x) (not (member x '("." "..")))))
           (strip-ext-fn (lambda (x) (s-left -3 x)))
           (all-files (directory-files "~/.spacemacs.d/packages/"))
           (package-files-ext (-filter filter-fn all-files))
           (package-files (-map strip-ext-fn package-files-ext))
           (package-symbols (-map 'read package-files))
           (package-requires (-map 'require-template package-symbols)))
      `(progn ,@package-requires)))

  (setq diff-hl-side 'left
        git-gutter-fr+-side 'left-fringe
        alchemist-test--mode-name-face nil
        x-select-enable-clipboard nil
        indent-buffer-modes '(scss-mode)
        projectile-use-git-grep t
        scroll-error-top-bottom t
        helm-dash-browser-func 'eww-other-window
        helm-dash-docsets-path "~/.docsets"
        helm-make-named-buffer t
        linum-format (quote "%4d "))

  (setq neo-smart-open nil
        neo-dont-be-alone nil
        neo-persist-show nil
        neo-show-hidden-files nil
        neo-auto-indent-point nil
        neo-modern-sidebar nil)

  (setq twittering-icon-mode t
        ;; Use master password for twitter instead of authenticating every time
        twittering-cert-file "/etc/ssl/certs/ca-bundle.crt"
        twittering-use-master-password t
        twittering-convert-fix-size 24)

  (bind-key* "C-S-V" 'simpleclip-paste)
  (bind-key* "C-S-C" 'simpleclip-copy)
  (bind-key* "C-S-X" 'simpleclip-cut)
  (bind-key* "M-+" 'align-regexp)
  (bind-key* "M-C" 'capitalize-previous-word)
  (bind-key* "M-1" 'select-window-1)
  (bind-key* "M-2" 'select-window-2)
  (bind-key* "M-3" 'select-window-3)
  (bind-key* "M-4" 'select-window-4)
  (bind-key* "M-5" 'select-window-5)
  (bind-key* "M-6" 'select-window-6)
  (bind-key* "M-7" 'select-window-7)
  (bind-key* "M-8" 'select-window-8)
  (bind-key* "M-9" 'select-window-9)

  (load-my-packages)

  (define-key evil-normal-state-map (kbd "<SPC>qq") 'undefined)
  (evil-define-key 'normal helm-map (kbd "<RET>") 'my/helm-exit-minibuffer)
  (key-chord-define-global "xs" 'evil-normal-state-and-save)

  (add-hook* 'twittering-mode-hook (setq-local mode-line-format nil))
  (add-hook* 'clojure-mode-hook (setq-local helm-dash-docsets '("Clojure")))
  (add-hook* 'elixir-mode-hook (setq-local helm-dash-docsets '("Elixir")))
  (add-hook* 'emacs-lisp-mode-hook (setq-local helm-dash-docsets '("Emacs Lisp")))
  (add-hook* 'erlang-mode-hook (setq-local helm-dash-docsets '("Erlang")))
  (add-hook* 'haskell-mode-hook (setq-local helm-dash-docsets '("Haskell")))
  (add-hook* 'ruby-mode-hook (setq-local helm-dash-docsets '("Ruby")))
  (add-hook* 'rust-mode-hook (setq-local helm-dash-docsets '("Rust")))
  (add-hook* 'prog-mode-hook (key-chord-mode 1))
  (add-hook* 'isearch-mode-hook (key-chord-mode 1))
  (add-hook* 'before-save-hook (indent-buffer-on-save)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-cons-mode-line-p nil)
 '(anzu-mode-line-update-function (quote spacemacs/anzu-update-mode-line))
 '(auto-compile-mode-line-counter t)
 '(custom-safe-themes
   (quote
    ("38f48e62e16e2c8f178c7e9de00aab382bc92d84ea382822907ed4e762388ae0" "84a0e9f3a2e3c239bcbfc5bdbd7c2ddd73e029948d47e1b14d955a0f72e78f80" "b274a26505b49a575482495a58601778be3d9f6bfab6a0463398907947c5c929" "16241cf61a64014901a8dfb661cff9e5d9702f743f4e888c26de8c92267242d6" "7c0f19a98f44505031ceeba97a53500a5be3b435e77f5b68d7a4d0e24d26d409" "82f76cd91d90fa8efa13d0d2eedd89dc1a3d0395aaa53323c652adb76b5ec294" "e4cd51174fa94cb07992e7ac685cab49db6682e9ff2e860113002ed3cc950aa6" "838f2f0ac542dae7e43d27902340eea41f00ac8e422632d887ed654131997d42" "b833c803c37a6b17e91e2152b9da4618302af50c7e1644b3a395ab162676d5a8" "cc67c4d5fcd37a750975cd50fb2555c9654dc5b92b6fb04d65161bdc4d708b9b" "6bc2bb2b8de7f68df77642b0615d40dc7850c2906b272d3f83a511f7195b07da" "b317b64ade8a19383695b1331496e80ae9117cfa57ab5287c436ceeded021d4b" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(elm-format-on-save t)
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(erc-image-inline-rescale 1)
 '(evil-want-Y-yank-to-eol t)
 '(global-hl-line-mode t)
 '(global-page-break-lines-mode t)
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-make-comint t)
 '(neo-enter-hook (quote (ignore)))
 '(neo-persist-show t t)
 '(neo-show-hidden-files nil t)
 '(neo-theme (quote ascii))
 '(package-selected-packages
   (quote
    (org-projectile org-download link-hint git-link flycheck-mix eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump column-enforce-mode clojure-snippets cargo clj-refactor inflections edn multiple-cursors paredit peg cider-eval-sexp-fu cider queue clojure-mode powerline rust-mode spinner alert log4e gntp markdown-mode parent-mode projectile request gitignore-mode fringe-helper git-gutter+ git-gutter flx magit magit-popup git-commit with-editor smartparens iedit anzu highlight f s doom-themes all-the-icons deferred pos-tip yasnippet packed company dash elixir-mode pkg-info epl avy async auto-complete popup package-build bind-key bind-map evil transmission flycheck helm helm-core hydra zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key volatile-highlights uuidgen use-package twittering-mode toml-mode toc-org spacemacs-theme spaceline smooth-scrolling smeargle simpleclip shell-pop scss-mode ruby-end restart-emacs ranger rainbow-delimiters racer quelpa popwin persp-mode pcre2el paradox page-break-lines orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme key-chord info+ indent-guide ido-vertical-mode hungry-delete htmlize hlinum hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md geiser flycheck-rust flycheck-pos-tip flycheck-elm flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks elm-mode elisp-slime-nav dockerfile-mode diff-hl define-word company-statistics company-racer company-quickhelp clean-aindent-mode buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(spaceline-helm-mode t)
 '(spaceline-info-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alchemist-test--failed-face ((t (:inherit font-lock-variable-name-face :foreground "tomato" :weight bold))))
 '(alchemist-test--success-face ((t (:inherit font-lock-variable-name-face :foreground "green" :weight bold))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(elscreen-tab-background-face ((t (:background "#1f252b"))))
 '(elscreen-tab-control-face ((t (:background "#1f252b" :foreground "#1f252b"))))
 '(elscreen-tab-current-screen-face ((t (:background "#262c34" :foreground "#B5BABF"))))
 '(elscreen-tab-other-screen-face ((t (:background "#353a42" :foreground "#1e2022"))))
 '(header-line ((t (:background "#1f252b")))))
