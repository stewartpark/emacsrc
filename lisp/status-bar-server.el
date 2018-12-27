;;; status-bar-server.el --- Status bar generator server for dzen2
;;;
;;; Commentary:
;;; Status bar generator server generates text for dzen2 via TCP
;;;
;;; Code:

(defun status-bar-widget--datetime ()
  (concat
   "^fg(#f403a0)^fn(Symbola)⏰^fn() "
   (current-time-string)
   "^fg()"))

(defun status-bar-widget--ethernet ()
  (let ((name (get-ethernet-name)))
    (concat
     "^fg(#0fa)"
     "^fn(Symbola)🖳^fn() "
     (if (eq (length name) 0)
       "^fn(Symbola)^fg(red)🚫^fg()^fn()"
       (concat name "(" (get-net-ipv4-addr name) ")"))
     "^fg()")))

(defun status-bar-widget--wifi ()
  (let ((name (get-wifi-name)))
    (concat
     "^fg(#0fa0b0)"
     "^fn(Symbola)📶^fn() "
     (if (eq (length name) 0)
       "^fn(Symbola)^fg(red)🚫^fg()^fn()"
       (concat name "(" (get-net-ipv4-addr name) ")"))
     "^fg()")))

(defun status-bar-widget--vpn ()
  (let ((name (get-vpn-name)))
    (concat
     "^fg(#00aa00)^fn(Symbola)🏢^fn() "
     (if (eq (length name) 0)
      "^fn(Symbola)^fg(red)🚫^fg()^fn()"
      (concat name "(" (get-net-ipv4-addr name) ")")
      )
     "^fg()")))

(defun status-bar-widget--volume ()
  (concat
   "^fg(#0050a0)^fn(Symbola)🔊^fn() "
   (desktop-environment-volume-get)
   "^fg()"))

(defun status-bar-widget--brightness ()
  (concat
   "^fg(yellow)^fn(Symbola)🔆^fn() "
   (desktop-environment-brightness-get)
   "^fg()"))

(defun status-bar-widget--battery ()
  (let ((gov (trim-string (file:read "/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor")))
        (status (trim-string (file:read "/sys/class/power_supply/BAT0/status")))
        (capacity (trim-string (file:read "/sys/class/power_supply/BAT0/capacity"))))
    (concat
     "^fg(#00f0ff)"
     "^fn(Symbola)"
     (if (string= status "Discharging") "" "⚡")
     (if (string= gov "performance") "💪" "🔋")
     "^fn() "
     capacity "%"
     "^fg()")))

(defun status-bar-server--filter (proc string)
  (when (string= string "dzen2\n")
    (process-send-string
     proc
     (concat
      "(λx.^r(18x1) "
      (status-bar-widget--datetime) " | "
      (status-bar-widget--ethernet) " | "
      (status-bar-widget--wifi) " | "
      (status-bar-widget--vpn) " | "
      (status-bar-widget--volume) " | "
      (status-bar-widget--brightness) " | "
      (if (file-exists-p "/sys/class/power_supply/BAT0/status")
          (status-bar-widget--battery) "")
      " ^r(18x1))"
      "\n"))))

(get-net-latency)

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
