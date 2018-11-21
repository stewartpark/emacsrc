;;; status-bar-server.el --- Status bar generator server for dzen2
;;;
;;; Commentary:
;;; Status bar generator server generates text for dzen2 via TCP
;;;
;;; Code:

(defun status-bar-server--filter (proc string)
  (when (string= string "dzen2\n")
    (process-send-string proc (concat
                               "^fn(Inconsolata)"
                               "^fg(#f44336)" (current-time-string) " ^fg()"))))

(defun status-bar-server-start ()
  (interactive)
  (make-network-process
   :name "status-bar"
   :buffer "*StatusBarServer*"
   :server t
   :family 'ipv4
   :host "127.0.0.1"
   :service 9999
   :filter 'status-bar-server--filter))

(defun status-bar-server-stop ()
  (interactive)
  (delete-process "status-bar"))

(provide 'status-bar-server)
;;; status-bar-server.el ends here
