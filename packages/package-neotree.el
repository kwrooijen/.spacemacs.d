  (use-package neotree
    :config
    (setq neo-force-change-root t
          neo-toggle-window-keep-p t)

    (defun neotree-projectile-highlight-file ()
      (interactive)
      (when (projectile-project-p)
        (let ((n (window-numbering-get-number))
              (origin-buffer-file-name (buffer-file-name)))
          (neo-global--open-dir (projectile-project-root))
          (when origin-buffer-file-name
            (neotree-find origin-buffer-file-name)
            (hl-line-mode 1)
            (when buffer-file-name
              (setq default-directory (file-name-directory buffer-file-name))))
        (select-window-by-number n))))

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

    (defadvice elscreen-goto (before elscreen-goto activate)
      (progn
        (when (get-buffer " *NeoTree*")
          (neotree-hide))))

    (defadvice elscreen-goto (after elscreen-goto activate)
      (progn
        (neotree-projectile-highlight-file)))

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
     select-window-9))

(provide 'package-neotree)
