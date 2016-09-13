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

(provide 'package-erc)
