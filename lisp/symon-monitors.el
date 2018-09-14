;;; symon-mintors.el --- Custom symon monitors and config.
;;; Commentary:
;;; Code:

(require 'symon)

(setq network-interfaces (network-interface-list))

(define-symon-monitor current-time-monitor
  :display (format-time-string "%F %I:%M%p |"))

(define-symon-monitor network-interface-monitor
  :display (let
               ((l (make-list 0 nil)))
             (progn
               (dolist
                   (n network-interfaces)
                 (when (or (string-prefix-p "wl" (car n))
                           (string-prefix-p "en" (car n))
                           (string-prefix-p "eth" (car n))
                           (string-prefix-p "tun" (car n)))
                 (seq-let
                     [ip0 ip1 ip2 ip3] (cdr n)
                   (add-to-list 'l (format " | %s(%d.%d.%d.%d)" (car n) ip0 ip1 ip2 ip3)))))
               (apply 'concat l))))

(setq symon-monitors
  (cond ((memq system-type '(gnu/linux cygwin))
         '(current-time-monitor
           symon-linux-battery-monitor
           symon-linux-memory-monitor
           symon-linux-cpu-monitor
           symon-linux-network-rx-monitor
           symon-linux-network-tx-monitor
           network-interface-monitor))
        ((memq system-type '(darwin))
         '(current-time-monitor
           symon-darwin-battery-monitor
           symon-darwin-memory-monitor
           symon-darwin-cpu-monitor
           symon-darwin-network-rx-monitor
           symon-darwin-network-tx-monitor
           network-interface-monitor))
        ((memq system-type '(windows-nt))
         '(current-time-monitor
           symon-windows-battery-monitor
           symon-windows-memory-monitor
           symon-windows-cpu-monitor
           symon-windows-network-rx-monitor
           symon-windows-network-tx-monitor
           network-interface-monitor))))

(provide 'symon-monitors)
;;; symon-monitors.el ends here
