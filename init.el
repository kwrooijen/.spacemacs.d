;; Debug Emacs freeze:
;; kill -SIGUSR2 $emacs-pid
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(sql
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     (clojure :variables
              clojure-enable-clj-refactor t
              clojure-enable-sayid t
              clojure-enable-linters '(clj-kondo joker))
     docker
     emacs-lisp
     git
     html
     javascript
     markdown
     syntax-checking
     version-control
     yaml)

   dotspacemacs-additional-packages '(all-the-icons
                                      flycheck-joker
                                      flycheck-clj-kondo
                                      flycheck-clojure
                                      doom-modeline
                                      doom-themes
                                      key-chord
                                      lispy
                                      lispyville
                                      scss-mode
                                      simpleclip
                                      solaire-mode
                                      yasnippet-snippets
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
                dotspacemacs-themes '(doom-spacegrey)
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
                dotspacemacs-mode-line-unicode-symbols nil
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

  (key-chord-mode 1)

  (setq anzu-cons-mode-line-p nil
        anzu-mode-line-update-function 'spacemacs/anzu-update-mode-line
        diff-hl-side 'left
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
        shell-pop-full-span nil
        evil-want-fine-undo t
        css-indent-offset 2
        python-shell-interpreter "/usr/bin/python3"
        ispell-program-name "aspell")

  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
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
  ;; (setq dotspacemacs-mode-line-unicode-symbols nil)
  ;; (setq ediff-window-setup-function 'ediff-setup-windows-default)
  ;; (set-selection-coding-system nil)

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
