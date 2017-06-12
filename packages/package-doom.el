(require 'doom-themes)

;;; Settings (defaults)
(setq doom-enable-bold t    ; if nil, bolding are universally disabled
      doom-enable-italic t  ; if nil, italics are universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil)

;; Load the theme (doom-one, doom-dark, etc.)
(load-theme 'doom-one t)

;; ;;; OPTIONAL
;; ;; brighter source buffers (that represent files)
;; (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
;; ;; if you use auto-revert-mode
;; (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
;; ;; you can brighten other buffers (unconditionally) with:
;; (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)


(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; You can do similar with the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
(provide 'package-doom)
