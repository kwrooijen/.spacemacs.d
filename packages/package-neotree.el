(require 'neotree)

(setq neo-auto-indent-point nil
      neo-dont-be-alone nil
      neo-modern-sidebar nil
      neo-persist-show nil
      neo-show-hidden-files nil
      neo-smart-open nil
      neo-theme 'icons
      neo-force-change-root t)

(defvar neo--ignored-list
  '(minibuffer-inactive-mode
    helm-major-mode
    messages-buffer-mode
    spacemacs-buffer-mode
    help-mode
    eshell-mode
    fundamental-mode
    magit-status-mode
    neotree-mode
    Buffer-menu-mode))

(defvar neo--allowed-command-list
  '(helm-ff-RET
    ido-exit-minibuffer))

(defvar neo--allowed-command-list-no-delay
  '(winum-select-window-0
    winum-select-window-1
    winum-select-window-2
    winum-select-window-3
    winum-select-window-4
    winum-select-window-5
    winum-select-window-6
    winum-select-window-7
    winum-select-window-8
    winum-select-window-9))

(defun unix-timestamp ()
  (read (format-time-string "%s")))

(defvar neo--current-buffer nil)

(defvar neo--last-time (unix-timestamp))

(defvar neo--debug nil)

(defvar neo--delay 1)

(add-hook 'neotree-mode-hook
          (lambda ()
            (with-current-buffer (get-buffer neo-buffer-name)
              (setq-default cursor-in-non-selected-windows nil)
              (set-window-fringes (get-buffer-window neo-buffer-name) 0 0 nil))

            ;; Remove NeoTree Banner
            (defun neo-buffer--insert-banner ())

            ;; Add ONLY project base name. On top of neotree
            (defun neo-buffer--insert-root-entry (node)
              (let* ((path (split-string node "/"))
                     (path (remove-if (lambda (x) (equal "" x)) path))
                     (path (car (last path))))
                (neo-buffer--insert-with-face (or path "NeoTree")
                                              'neo-root-dir-face)
                (neo-buffer--newline-and-begin)))))

(defun neo--allowed-command? ()
  (or (member this-command neo--allowed-command-list-no-delay)
      (and
       (member this-command neo--allowed-command-list)
       (< (+ neo--delay neo--last-time) (unix-timestamp)))))

;; Refresh neotree on buffer change
(defadvice select-window (after select-window activate)
  (when (and (projectile-project-p)
             (not (eql (current-buffer) neo--current-buffer))
             (not (member major-mode neo--ignored-list))
             (neo--allowed-command?))
    (setq neo--last-time (unix-timestamp))
    (setq neo--current-buffer (current-buffer))
    (when neo--debug
      (message (format "MAJOR MODE: %s | BUFFER: %s | THIS COMMAND: %s"
                       major-mode
                       (buffer-name)
                       this-command)))
    (neotree-refresh t)
    (with-current-buffer (get-buffer neo-buffer-name)
      (hl-line-mode t)
      (internal-show-cursor (get-buffer-window neo-buffer-name) nil)
      (set-window-fringes (get-buffer-window neo-buffer-name) 0 0 nil))))

(provide 'package-neotree)
