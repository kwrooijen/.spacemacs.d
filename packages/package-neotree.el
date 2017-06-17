(add-hook 'neotree-mode-hook (lambda ()
                               (setq-default cursor-in-non-selected-windows nil)
                               (defun neotree-refresh (&optional is-auto-refresh)
                                 "Refresh the NeoTree buffer."
                                 (interactive)
                                 (if (eq (current-buffer) (neo-global--get-buffer))
                                     (neo-buffer--refresh t)
                                   (save-excursion
                                     (let ((cw (selected-window)))  ;; save current window
                                       (if is-auto-refresh
                                           (let ((origin-buffer-file-name (buffer-file-name)))
                                             (when (and (fboundp 'projectile-project-p)
                                                        (projectile-project-p)
                                                        (fboundp 'projectile-project-root))
                                               (neo-global--open-dir (projectile-project-root))
                                               (neotree-find (projectile-project-root)))
                                             (neotree-find origin-buffer-file-name))
                                         (neo-buffer--refresh t t))
                                       (recenter)
                                       (hl-line-mode t)
                                       (internal-show-cursor (get-buffer-window " *NeoTree*") nil)
                                       (when (or is-auto-refresh neo-toggle-window-keep-p)
                                         (select-window cw))))))))


(defun update-neo-tree-for-template (fun)
  `(defadvice ,fun (after ,fun activate)
     (neotree-refresh t)))

(defmacro update-neo-tree-for (&rest funs)
  (let ((forms (mapcar 'update-neo-tree-for-template funs)))
    `(progn ,@forms)))

(update-neo-tree-for helm-projectile-find-file
                     helm-projectile-switch-project
                     helm-mini
                     ido-kill-buffer
                     spacemacs/helm-find-files
                     winner-undo
                     winner-redo
                     dired-find-file
                     select-window-1
                     select-window-2
                     select-window-3
                     select-window-4
                     select-window-5
                     select-window-6
                     select-window-7
                     select-window-8
                     select-window-9)

(with-eval-after-load "neotree")


(provide 'package-neotree)
