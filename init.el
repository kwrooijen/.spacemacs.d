(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(sql
     html
     c-c++
     dash
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     elixir
     erlang
     python
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
   dotspacemacs-additional-packages '(solaire-mode
                                      all-the-icons
                                      key-chord
                                      scss-mode
                                      uuidgen
                                      simpleclip
                                      lispy
                                      lispyville
                                      rainbow-mode
                                      powerline
                                      flycheck-clojure
                                      flycheck-pos-tip
                                      string-inflection
                                      clojure-cheatsheet
                                      evil-anzu
                                      ov
                                      hy-mode
                                      deferred
                                      arduino-mode
                                      platformio-mode
                                      eldoc-eval)
   dotspacemacs-excluded-packages '(evil-escape linum multiple-cursors spaceline)
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
                dotspacemacs-themes '(doom-one spacemacs-dark spacemacs-light)
                dotspacemacs-colorize-cursor-according-to-state t
                dotspacemacs-default-font '("Fira Mono"
                                            :size 13
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

  (load-my-packages)

  (add-hook* 'prog-mode-hook (key-chord-mode 1))
  (add-hook* 'isearch-mode-hook (key-chord-mode 1))

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
        x-select-enable-clipboard nil
        shell-pop-full-span nil
        evil-want-fine-undo t
        css-indent-offset 2)

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

  (load-file "~/.spacemacs.d/modeline.el")

  (when (file-exists-p "~/.spacemacs.d/variables.el")
    (load-file "~/.spacemacs.d/variables.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0d0d0d" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#c9b4cf" "#8abeb7" "#ffffff"])
 '(custom-safe-themes
   (quote
    ("77bddca0879cb3b0ecdf071d9635c818827c57d69164291cb27268ae324efa84" "a7e7804313dbf827a441c86a8109ef5b64b03011383322cbdbf646eb02692f76" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "6b1e6953a08acf12843973ec25d69dbfa1a53d869f649dc991a56fbdf0d7eb9e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ead76c417365064889c6552678e62a3982f9c6b359888dd7b2ba62efb9422b96" default)))
 '(erlang-argument-indent 2)
 '(erlang-indent-level 2)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#5c5e5e" t)
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#41728e"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (know-your-http-well httpcode eldoc-eval pyvenv live-py-mode cython-mode all-the-icons anaconda-mode org-plus-contrib ghub pythonic org-mime erlang platformio-mode arduino-mode deferred markdown-preview-mode ov disaster company-c-headers cmake-mode clang-format cljr-helm elein parinfer auto-auto-indent flycheck-clojure clojure-cheatsheet helm-clojuredocs auto-indent-mode evil-lispy solaire-mode string-inflection racket-mode faceup orgit sublimity mini-header-line dired+ vline winum fuzzy flycheck-credo evil-multiedit doom-themes powerline alert log4e gntp org markdown-mode skewer-mode simple-httpd lispy zoutline swiper ivy js2-mode parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flycheck flx magit git-commit with-editor smartparens iedit anzu evil goto-chg f json-mode tablist magit-popup docker-tramp json-snatcher json-reformat diminish web-completion-data dash-functional tern pos-tip hydra inflections edn multiple-cursors paredit s peg eval-sexp-fu highlight cider seq spinner queue clojure-mode rust-mode bind-map bind-key yasnippet packed company dash elixir-mode pkg-info epl ace-window helm avy helm-core async auto-complete popup package-build rainbow-mode zeal-at-point yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode simpleclip shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters racer quelpa pug-mode popwin persp-mode pcre2el paradox org-projectile org-present org-pomodoro org-download org-bullets open-junk-file ob-elixir neotree multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode lispyville linum-relative link-hint less-css-mode key-chord js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md geiser flycheck-rust flycheck-pos-tip flycheck-mix flycheck-elm flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode elm-mode elisp-slime-nav dumb-jump dockerfile-mode docker diff-hl define-word company-web company-tern company-statistics company-quickhelp column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cargo auto-yasnippet auto-highlight-symbol auto-compile alchemist adaptive-wrap ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(vc-annotate-background "#0d0d0d")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b5bd68")
    (cons 40 "#c8c06c")
    (cons 60 "#dcc370")
    (cons 80 "#f0c674")
    (cons 100 "#eab56d")
    (cons 120 "#e3a366")
    (cons 140 "#de935f")
    (cons 160 "#d79e84")
    (cons 180 "#d0a9a9")
    (cons 200 "#c9b4cf")
    (cons 220 "#ca9aac")
    (cons 240 "#cb8089")
    (cons 260 "#cc6666")
    (cons 280 "#af6363")
    (cons 300 "#936060")
    (cons 320 "#765d5d")
    (cons 340 "#5c5e5e")
    (cons 360 "#5c5e5e")))
 '(vc-annotate-very-old-color nil))
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
