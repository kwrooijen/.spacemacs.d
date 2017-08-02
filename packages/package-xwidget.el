(require 'xwidget)

(defun xwidget-webkit-reload-cache ()
  (interactive)
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 "location.reload(true);"))

(evil-set-initial-state 'xwidget-webkit-mode 'emacs)

(defconst webkit+--input-event
  "var evt = document.createEvent('Events');
   evt.initEvent('keypress', true, true);

   evt.view = window;
   evt.altKey = false;
   evt.ctrlKey = false;
   evt.shiftKey = false;
   evt.metaKey = false;
   evt.keyCode = 0;
   evt.charCode = '%s'.charCodeAt(0);

   document.dispatchEvent(evt);")

(defconst webkit+--keycodes
  (mapcar 'char-to-string (number-sequence 33 126 1)))

;; (defvar console-messages '())

;; (defun get-js-console ()
;;   (xwidget-webkit-execute-script-rv (xwidget-webkit-current-session)
;;                                     "emacsLogs.pop();"))


;; (deferred:$
;;   (deferred:next
;;     (deferred:lambda (x)
;;       (let ((msg (get-js-console)))
;;         (when (not (equal msg "undefined"))
;;           (push msg console-messages)))
;;       (when (not kill-it)
;;         (deferred:nextc (deferred:wait 1000) self)))))

;; (setq kill-it nil)

;; console-messages
;; (xwidget-webkit-execute-script (xwidget-webkit-current-session)
;;                                "
;; var emacsLogs = [];
;; function yourCustomLog(msg) {
;;   emacsLogs.push(msg);
;; }
;; window.console.log = yourCustomLog;")

;; (xwidget-webkit-execute-script (xwidget-webkit-current-session)
;;                                "console.log(new Object);")
                                        ; typeof(object)
                                        ; Object.keys(object)


(defun xwidget-webkit-self-insert-command (c)
  (interactive (list (this-command-keys)))
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 (format webkit+--input-event c)))


(add-hook 'xwidget-webkit-mode-hook
          (lambda ()
            (define-key xwidget-webkit-mode-map (kbd "C-r") 'xwidget-webkit-reload-cache)
            (setq-local mode-line-format nil)
            (add-hook 'window-configuration-change-hook
                      (lambda ()
                        (when (equal major-mode 'xwidget-webkit-mode)
                          (xwidget-webkit-adjust-size-dispatch))))
            (mapcar (lambda (c)
                      (define-key xwidget-webkit-mode-map (kbd c) 'xwidget-webkit-self-insert-command))
                    webkit+--keycodes)))

(provide 'package-xwidget)
