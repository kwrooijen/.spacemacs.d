(use-package elscreen
    :load-path "~/.spacemacs.d/elscreen/"
    :init
    (setq elscreen-tab-separation-width 1.5)
    (ensure-clone "kwrooijen" "elscreen" "add-ignore-list")
    :config
    (elscreen-start)
    (elscreen-create)
    (elscreen-create)
    (elscreen-create)
    (elscreen-create)
    (elscreen-kill 0)
    (elscreen-goto 1)
    (switch-to-buffer "*spacemacs*")
    (defun elscreen-goto-1 ()
      (interactive)
      (elscreen-goto 1))
    (defun elscreen-goto-2 ()
      (interactive)
      (elscreen-goto 2))
    (defun elscreen-goto-3 ()
      (interactive)
      (elscreen-goto 3))
    (defun elscreen-goto-4 ()
      (interactive)
      (elscreen-goto 4))

    (evil-leader/set-key
      "1" 'elscreen-goto-1
      "2" 'elscreen-goto-2
      "3" 'elscreen-goto-3
      "4" 'elscreen-goto-4))

(provide 'package-elscreen)
