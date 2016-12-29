(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(html
     auto-completion
     (dash :variables
           dash-helm-dash-docset-path "~/.docsets")
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
     spacemacs-layouts
     (shell :variables
            shell-default-height 20
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-protect-eshell-prompt t)
     ;; spell-checking
     version-control)
   dotspacemacs-additional-packages '(key-chord scss-mode uuidgen simpleclip darktooth-theme lispy lispyville)
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
   dotspacemacs-themes '(darktooth
                         spacemacs-dark
                         solarized-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai
                         zenburn)
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
        helm-make-named-buffer t)

  (setq neo-smart-open nil
        neo-dont-be-alone nil
        neo-persist-show nil
        neo-show-hidden-files nil
        neo-auto-indent-point nil
        neo-modern-sidebar nil)

  (bind-key* "C-S-V" 'simpleclip-paste)
  (bind-key* "C-S-C" 'simpleclip-copy)
  (bind-key* "C-S-X" 'simpleclip-cut)
  (bind-key* "M-u" 'undo-tree-redo)
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

  (spaceline-toggle-minor-modes-off)

  (load-my-packages)

  (define-key evil-normal-state-map (kbd "<SPC>qq") 'undefined)
  (evil-define-key 'normal helm-map (kbd "<RET>") 'my/helm-exit-minibuffer)
  (key-chord-define-global "xs" 'evil-normal-state-and-save)

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
 '(elm-format-on-save t)
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(erc-image-inline-rescale 1)
 '(erc-prompt-for-password nil)
 '(evil-want-Y-yank-to-eol t)
 '(global-hl-line-mode t)
 '(global-page-break-lines-mode t)
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-make-comint t)
 '(package-selected-packages
   (quote
    (ruby-end zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode simpleclip shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters racer quelpa pug-mode popwin persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-elixir neotree multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode lispyville linum-relative link-hint less-css-mode key-chord js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md geiser flycheck-rust flycheck-pos-tip flycheck-mix flycheck-elm flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode elm-mode elisp-slime-nav dumb-jump dockerfile-mode docker diff-hl define-word darktooth-theme company-web company-tern company-statistics company-quickhelp column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cargo auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adaptive-wrap ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(projectile-enable-caching t)
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
 '(header-line ((t (:inherit (quote default) :background nil)))))
