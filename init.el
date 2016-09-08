;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     (dash :variables
           dash-helm-dash-docset-path "~/.docsets")
     elixir
     elm
     emacs-lisp
     erc
     dockerfile
     git
     syntax-checking
     eyebrowse
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
     ;; syntax-checking
     version-control)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(key-chord scss-mode twittering-mode uuidgen hlinum)
   ;; A LIST of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(vi-tilde-fringe)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 11
                               :weight bold
                               :width normal
                               :powerline-scale 1.7)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "C-M-]"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key nil
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

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

  (setq diff-hl-side 'left
        git-gutter-fr+-side 'left-fringe
        x-select-enable-clipboard nil
        indent-buffer-modes '(scss-mode))

  (defun indent-buffer-on-save ()
    (if (member major-mode indent-buffer-modes)
        (spacemacs/indent-region-or-buffer)))

  (setq x-select-enable-clipboard nil
        projectile-use-git-grep t
        scroll-error-top-bottom t
        helm-dash-browser-func 'eww-other-window
        helm-dash-docsets-path "~/.docsets"
        alchemist-test--mode-name-face nil
        helm-make-named-buffer t
        linum-format (quote "%4d "))

  (setq twittering-icon-mode t
        ;; Use master password for twitter instead of authenticating every time
        twittering-cert-file "/etc/ssl/certs/ca-bundle.crt"
        twittering-use-master-password t
        twittering-convert-fix-size 24)

  (bind-key* "C-S-V" 'x-clipboard-yank)
  (bind-key* "C-S-C" 'clipboard-kill-ring-save)
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

  (require 'magit)
  (define-key magit-mode-map "\M-1" 'select-window-1)
  (define-key magit-mode-map "\M-2" 'select-window-2)
  (define-key magit-mode-map "\M-3" 'select-window-3)
  (define-key magit-mode-map "\M-4" 'select-window-4)
  (define-key magit-mode-map "\M-5" 'select-window-5)
  (define-key magit-mode-map "\M-6" 'select-window-6)
  (define-key magit-mode-map "\M-7" 'select-window-7)
  (define-key magit-mode-map "\M-8" 'select-window-8)
  (define-key magit-mode-map "\M-9" 'select-window-9)
  (define-key evil-normal-state-map (kbd "<SPC>qq") 'undefined)

  (use-package multiple-cursors
    :load-path "~/.spacemacs.d/multiple-cursors.el/"
    :bind* (("M-K" . mc/mark-previous-like-this)
            ("M-J" . mc/mark-next-like-this))
    :bind (:map mc/keymap
                ("<return>" . newline))
    :init
    (ensure-clone "myrjola" "multiple-cursors.el" "evil-compat")
    :config
    (multiple-cursors-mode t))

  (use-package neotree
    :config
    (defun neotree-find-project-root-no-jump ()
      (interactive)
      (let ((c (current-buffer))
            (origin-buffer-file-name (buffer-file-name)))
        (neotree-find (projectile-project-root))
        (neotree-find origin-buffer-file-name)
        (hl-line-mode 1)
        (switch-to-buffer c)
        (setq default-directory (file-name-directory buffer-file-name))))

    (defadvice helm-projectile-find-file (after helm-projectile-find-file activate)
      (neotree-find-project-root-no-jump))
    (defadvice helm-projectile-switch-project (after helm-projectile-switch-project activate)
      (neotree-find-project-root-no-jump)))

  (use-package doom-theme
    :load-path "~/.spacemacs.d/emacs-doom-theme/"
    :config
    (require 'f)
    (load-file "~/.spacemacs.d/modeline.el")

    (defun doom*neo-insert-root-entry (node)
      "Pretty-print pwd in neotree"
      (list (concat "  " (projectile-project-name))))

    (defun doom*neo-insert-fold-symbol (name)
      "Custom hybrid unicode theme with leading whitespace."
      (or (and (eq name 'open)  (neo-buffer--insert-with-face " -  " 'neo-expand-btn-face))
          (and (eq name 'close) (neo-buffer--insert-with-face " +  " 'neo-expand-btn-face))
          (and (eq name 'leaf)  (neo-buffer--insert-with-face "   " 'neo-expand-btn-face))))

    (advice-add 'neo-buffer--insert-fold-symbol :override 'doom*neo-insert-fold-symbol)
    (advice-add 'neo-buffer--insert-root-entry :filter-args 'doom*neo-insert-root-entry)
    (load-theme 'doom-one)
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-attribute 'mode-line nil :box nil)
    :init
    (ensure-clone "hlissner" "emacs-doom-theme" "master"))

  (use-package hlinum
    :ensure t
    :config
    (set-face-attribute 'linum-highlight-face nil
                        :inherit 'hl-line-face
                        :background nil
                        :foreground "DodgerBlue")
    (hlinum-activate))

  (use-package fringe
    :config
    (setq-default fringes-outside-margins t)
    (set-fringe-mode '1)
    (add-hook 'prog-mode-hook 'linum-mode)
    (advice-add 'neo-global--select-window :after (lambda ()
                                                    (setq cursor-in-non-selected-windows nil)
                                                    (set-window-fringes neo-global--window 1 0)
                                                    (spacemacs/toggle-mode-line-off)))

    (defun small-fringe ()
      (if (not (active-minibuffer-window))
          (set-window-fringes (selected-window) 2 0 t)))

    (add-hook 'buffer-list-update-hook 'small-fringe)
    (set-face-foreground 'git-gutter-fr+-added "green")
    (set-face-background 'git-gutter-fr+-added "green")
    (set-face-foreground 'git-gutter-fr+-modified "yellow")
    (set-face-background 'git-gutter-fr+-modified "yellow")
    (set-face-foreground 'git-gutter-fr+-deleted "red")
    (set-face-background 'git-gutter-fr+-deleted "red")
    (set-face-background 'fringe "#262c34"))

  (use-package erc
    :bind (:map erc-mode-map
                ("C-M-m" . erc-send-current-line)
                ("RET" . erc-no-return))
    :config
    (setq
     erc-nick "kwrooijen"
     erc-scrolltobottom-mode t
     erc-hide-list '("JOIN" "PART" "QUIT"))
    :init
    (defadvice attic/erc (after attic-ad/attic/erc-after activate)
      (setq erc-password nil))
    (defun attic/erc ()
      (interactive)
      (load "~/.erc.gpg")
      (erc :server "irc.freenode.net"
           :port 6667
           :nick erc-nick
           :password erc-password))
    (defun erc-no-return ()
      (interactive)
      (message "Use C-M-m to send")))

  (spacemacs/toggle-mode-line-minor-modes-off)

  (evil-leader/set-key
    "1" 'eyebrowse-switch-to-window-config-1
    "2" 'eyebrowse-switch-to-window-config-2
    "3" 'eyebrowse-switch-to-window-config-3
    "4" 'eyebrowse-switch-to-window-config-4
    "5" 'eyebrowse-switch-to-window-config-5
    "6" 'eyebrowse-switch-to-window-config-6
    "7" 'eyebrowse-switch-to-window-config-7
    "8" 'eyebrowse-switch-to-window-config-8
    "9" 'eyebrowse-switch-to-window-config-9)

  (defun my/helm-exit-minibuffer ()
    (interactive)
    (helm-exit-minibuffer))
  (evil-define-key 'normal helm-map (kbd "<RET>") 'my/helm-exit-minibuffer)

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
  (add-hook* 'after-save-hook (indent-buffer-on-save))
  ;; Doesn't work
  (add-hook* 'prog-mode-hook (set-window-fringes (selected-window) 2 0 t))

  (key-chord-define-global "xs" 'evil-normal-state-and-save))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e4cd51174fa94cb07992e7ac685cab49db6682e9ff2e860113002ed3cc950aa6" "838f2f0ac542dae7e43d27902340eea41f00ac8e422632d887ed654131997d42" "b833c803c37a6b17e91e2152b9da4618302af50c7e1644b3a395ab162676d5a8" "cc67c4d5fcd37a750975cd50fb2555c9654dc5b92b6fb04d65161bdc4d708b9b" "6bc2bb2b8de7f68df77642b0615d40dc7850c2906b272d3f83a511f7195b07da" "b317b64ade8a19383695b1331496e80ae9117cfa57ab5287c436ceeded021d4b" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(elm-format-on-save t)
 '(evil-want-Y-yank-to-eol t)
 '(helm-make-comint t)
 '(neo-enter-hook (quote (ignore)))
 '(neo-persist-show t)
 '(neo-show-hidden-files nil)
 '(neo-theme (quote ascii))
 '(nyan-bar-length 14)
 '(nyan-mode t)
 '(paradox-github-token t)
 '(quote (paradox-github-token t)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alchemist-test--failed-face ((t (:inherit font-lock-variable-name-face :foreground "tomato" :weight bold))))
 '(alchemist-test--success-face ((t (:inherit font-lock-variable-name-face :foreground "green" :weight bold))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
