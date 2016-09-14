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
    (elscreen-create)
    (elscreen-kill 0)
    (elscreen-goto 1)
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
    (defun elscreen-goto-5 ()
      (interactive)
      (elscreen-goto 5))

    (evil-leader/set-key
      "1" 'elscreen-goto-1
      "2" 'elscreen-goto-2
      "3" 'elscreen-goto-3
      "4" 'elscreen-goto-4
      "5" 'elscreen-goto-5))

;; (setq-default powerline-text-scale-factor 20)

;; (defface epic
;;   '((t (:background "#262c34")))
;;   "")

;; (setq elscreen-tab-height 25)
;; (setq elscreen-tab-separator " ")

;; (defun powerline-iconic-theme ()
;;   "Setup the powerline-iconic theme"
;;   (interactive)
;;   (setq
;;    header-line-format
;;    '(" %e"
;;      (:eval
;;       (powerline-render (list
;;                          (powerline-wave-right nil 'epic elscreen-tab-height)
;;                          (powerline-raw " init.el " 'epic)
;;                          (powerline-wave-left 'epic nil elscreen-tab-height)

;;                          (powerline-raw elscreen-tab-separator)
;;                          (powerline-wave-right  nil 'epic elscreen-tab-height)
;;                          (powerline-raw " elscreen.el " 'epic)
;;                          (powerline-wave-left 'epic nil elscreen-tab-height)
;;                          ))))))

(provide 'package-elscreen)
