;; Debug Emacs freeze:
;; kill -SIGUSR2 $emacs-pid
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     sql
     ruby
     octave
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     (shell :variables
            shell-default-height 20
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-protect-eshell-prompt t)
     c-c++
     clojure
     csv
     dash
     docker
     elixir
     elm
     emacs-lisp
     erc
     erlang
     git
     html
     javascript
     lua
     markdown
     org
     python
     racket
     restclient
     rust
     scheme
     syntax-checking
     version-control
     yaml
     )
   dotspacemacs-additional-packages '(
                                      all-the-icons
                                      arduino-mode
                                      deferred
                                      doom-modeline
                                      doom-themes
                                      edit-indirect
                                      edts
                                      eldoc-eval
                                      evil-anzu
                                      flycheck-clojure
                                      flycheck-pos-tip
                                      hy-mode
                                      key-chord
                                      lispy
                                      lispyville
                                      org-brain
                                      ov
                                      platformio-mode
                                      powerline
                                      rainbow-mode
                                      scss-mode
                                      simpleclip
                                      solaire-mode
                                      spray
                                      string-inflection
                                      uuidgen
                                      yasnippet-snippets
                                      (godot-gdscript
                                       :location (recipe
                                                  :fetcher github
                                                  :repo "francogarcia/godot-gdscript.el"))
                                      (multiple-cursors :location (recipe :fetcher github
                                                                          :repo "jacobono/multiple-cursors.el"
                                                                          :branch "evil-ways")))
   dotspacemacs-excluded-packages '(evil-escape linum spaceline org-projectile)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default dotspacemacs-elpa-https t
                dotspacemacs-elpa-timeout 5
                dotspacemacs-check-for-update t
                dotspacemacs-editing-style 'vim
                dotspacemacs-verbose-loading nil
                dotspacemacs-startup-banner 'official
                dotspacemacs-startup-lists '(recents projects)
                dotspacemacs-startup-recent-list-size 5
                dotspacemacs-scratch-mode 'emacs-lisp-mode
                dotspacemacs-themes '(doom-one)
                dotspacemacs-colorize-cursor-according-to-state t
                dotspacemacs-default-font '("Fira Mono"
                                            :size 10
                                            :weight bold
                                            :width normal)
                dotspacemacs-leader-key "SPC"
                dotspacemacs-emacs-leader-key "C-M-]"
                dotspacemacs-major-mode-leader-key "'"
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
                dotspacemacs-helm-use-fuzzy 'source))

(defun dotspacemacs/user-init ()
  (add-to-load-path "~/.spacemacs.d/packages")
  (add-to-load-path "~/.spacemacs.d"))

(defun dotspacemacs/user-config ()
  (require 'functions)
  (require 'keys)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  (use-package simpleclip
    :config
    (simpleclip-mode 1))

  (load-my-packages)

  (when (eq system-type 'darwin)
    (bind-key* "<s-return>" 'toggle-frame-fullscreen))

  (key-chord-mode 1)

  (setq alchemist-test--mode-name-face nil
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
        projectile-enable-caching t
        projectile-use-git-grep t
        scroll-error-top-bottom t
        spaceline-helm-mode t
        spaceline-info-mode t
        shell-pop-full-span nil
        evil-want-fine-undo t
        css-indent-offset 2
        python-shell-interpreter "/usr/bin/python3"
        spray-margin-left 23
        spray-margin-top 5
        spray-wpm 600
        erlang-argument-indent 2
        erlang-indent-level 2
        ispell-program-name "aspell")

  (add-hook 'after-init-hook #'global-flycheck-mode)
  (spacemacs/toggle-auto-fill-mode-on)

  (fringe-mode '(nil . 0))

  (define-key global-map (kbd "C-x 1") 'delete-other-windows)

  (spacemacs/set-leader-keys
    "xht" 'hs-toggle-hiding
    "xhh" 'hs-hiding-block
    "xhs" 'hs-show-block
    "xha" 'hs-show-all
    "xhA" 'hs-hide-all
    "xhl" 'hs-hide-level)
  ;; Fix dired buffer issues
  (setq persp-set-frame-buffer-predicate nil)
  (setq persp-when-kill-switch-to-buffer-in-perspective nil)

  (setq persp-add-buffer-on-after-change-major-mode t)
  ;; Random things to hopefully prevent crashing
  (setq dotspacemacs-mode-line-unicode-symbols nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-default)
  (set-selection-coding-system nil)

  (add-hook* 'hy-mode-hook
             (define-key evil-normal-state-map (kbd "C-t") 'clojure-thread-first-all)
             (define-key evil-insert-state-map (kbd "C-t") 'clojure-thread-first-all)
             (define-key evil-normal-state-map (kbd "C-S-T") 'clojure-thread-last-all)
             (define-key evil-insert-state-map (kbd "C-S-T") 'clojure-thread-last-all))

  (when (file-exists-p "~/.spacemacs.d/variables.el")
    (load-file "~/.spacemacs.d/variables.el"))

  (require 'doom-modeline)
  (doom-modeline-mode 1)

  (define-key global-map [menu-bar buffers] nil)
  (define-key global-map [menu-bar file] nil)
  (define-key global-map [menu-bar help] nil)
  (define-key global-map [menu-bar perspective] nil)
  (define-key global-map [menu-bar options] nil)
  (define-key global-map [menu-bar edit] nil)
  (define-key global-map [menu-bar tools] nil)
  (define-key global-map [menu-bar projectile] nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-preferred-build-tool (quote shadow-cljs))
 '(custom-safe-themes
   (quote
    ("6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (helm-spotify-plus helm-spotify org-brain helm-jira sql-indent parseedn parseclj a pbcopy osx-clipboard doom-themes doom-theme-theme rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby swiper parent-mode projectile request flx highlight transient lv with-editor smartparens iedit anzu evil goto-chg undo-tree erc-terminal-notifier reformatter dash-at-point hydra sesman eval-sexp-fu rust-mode bind-map bind-key yasnippet auto-highlight-symbol packed spinner pythonic f s pkg-info epl ace-window helm avy helm-core async popup godot-gdscript org-category-capture alert log4e gntp skewer-mode simple-httpd lispy zoutline ivy js2-mode haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flycheck magit git-commit ghub treepy graphql erlang eldoc-eval shrink-path json-mode tablist magit-popup docker-tramp json-snatcher json-reformat web-completion-data dash-functional tern restclient know-your-http-well pos-tip inflections edn multiple-cursors paredit peg cider queue clojure-mode markdown-mode anaconda-mode all-the-icons memoize company elixir-mode auto-complete faceup solarized-theme spaceline evil-escape zeal-at-point yasnippet-snippets yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit string-inflection spray solaire-mode smeargle slim-mode simpleclip shell-pop scss-mode sass-mode restclient-helm restart-emacs rainbow-mode rainbow-delimiters racket-mode racer pyvenv pytest pyenv-mode py-isort pug-mode powerline popwin platformio-mode pip-requirements persp-mode pcre2el paradox ov orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file ob-restclient ob-http ob-elixir neotree multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode live-py-mode lispyville linum-relative link-hint key-chord js2-refactor js-doc jinja2-mode indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md geiser fuzzy flycheck-rust flycheck-pos-tip flycheck-mix flycheck-elm flycheck-credo flycheck-clojure flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode elm-mode elisp-slime-nav edts edit-indirect dumb-jump doom-modeline dockerfile-mode docker disaster diminish diff-hl define-word deferred cython-mode csv-mode company-web company-tern company-statistics company-restclient company-quickhelp company-c-headers company-ansible company-anaconda column-enforce-mode coffee-mode cmake-mode clojure-snippets clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu cargo auto-yasnippet auto-compile arduino-mode ansible-doc ansible alchemist aggressive-indent adaptive-wrap ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (auto-fill-mode . t)
     (flyspell-mode . t)
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
