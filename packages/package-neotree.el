  (use-package neotree
    :config
    (setq neo-force-change-root t
          neo-toggle-window-keep-p t)

    (defun neo-insert-root-entry (node)
      "Pretty-print pwd in neotree"
      (list (concat " ☰ " (projectile-project-name))))

    (defun neo-insert-fold-symbol (name)
      "Custom hybrid unicode theme with leading whitespace."
      (or (and (eq name 'open)  (neo-buffer--insert-with-face " - ⛉ " 'neo-expand-btn-face))
          (and (eq name 'close) (neo-buffer--insert-with-face " + ⛊ " 'neo-expand-btn-face))
          (and (eq name 'leaf)  (neo-buffer--insert-with-face "   " 'neo-expand-btn-face))))

    (advice-add 'neo-buffer--insert-fold-symbol :override 'neo-insert-fold-symbol)
    (advice-add 'neo-buffer--insert-root-entry :filter-args 'neo-insert-root-entry)

    (defun neotree-projectile-highlight-file ()
      (interactive)
      (when (projectile-project-p)
        (let ((n (window-numbering-get-number))
              (origin-buffer-file-name (buffer-file-name)))
          (when origin-buffer-file-name
            (neotree-find (projectile-project-root))
            (neotree-find origin-buffer-file-name)
            (hl-line-mode 1)
            (select-window-by-number n)
            (let ((dir (file-name-directory buffer-file-name)))
              (when dir
                (setq default-directory dir)))))))

    (defun update-neo-tree-for-template (fun)
      `(defadvice ,fun (after ,fun activate)
         (neotree-projectile-highlight-file)))

    (defmacro update-neo-tree-for (&rest funs)
      (let ((forms (mapcar 'update-neo-tree-for-template funs)))
        `(progn ,@forms)))

    (defun neotree-double-toggle ()
      (neotree-toggle)
      (neotree-toggle))

    (defadvice spacemacs/default-pop-shell (after spacemacs/default-pop-shell activate)
      (neotree-double-toggle))

    (defadvice compile (after compile activate)
      (neotree-double-toggle))

    (add-hook 'shell-mode-hook 'neotree-double-toggle)
    (add-hook 'comint-mode-hook 'no-split)
    (add-hook 'compile-mode-hook 'no-split)
    (add-hook 'eshell-mode-hook 'no-split)

    (update-neo-tree-for
     helm-projectile-find-file
     helm-projectile-switch-project
     helm-mini
     ido-kill-buffer
     spacemacs/helm-find-files
     winner-undo
     winner-redo
     select-window-1
     select-window-2
     select-window-3
     select-window-4
     select-window-5
     select-window-6
     select-window-7
     select-window-8
     select-window-9)

    (defun neo-tree-switch-window-template (num)
      (let ((fn (read (concat "eyebrowse-switch-to-window-config-" (number-to-string num)))))
        `(progn
           (defadvice ,fn (before ,fn activate)
             (neotree-hide))
           (defadvice ,fn (after ,fn activate)
             (progn
               ;; TODO Remember last window switched from
               (select-window-1)
               (neotree-projectile-highlight-file))))))

    (defmacro neo-tree-switch-window (&rest nums)
      (let ((forms (mapcar 'neo-tree-switch-window-template nums)))
        `(progn ,@forms)))

    (neo-tree-switch-window 1 2 3 4 5 6 7 8 9))

(provide 'package-neotree)
