(defmacro add-hook* (mode &rest body)
  `(add-hook ,mode (lambda () ,@body)))

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

(defun change-language ()
  (interactive)
  (save-excursion
    (call-interactively 'ispell-change-dictionary)
    (flyspell-buffer)))

(provide 'functions)
