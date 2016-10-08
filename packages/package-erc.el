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
  (defun erc-no-return ()
    (interactive)
    (message "Use C-M-m to send")))

(provide 'package-erc)
