;;; window-manager.el --- Make Emacs act as a window manager
;;;
;;; Commentary:
;;; Use exwm to make Emacs act as a window manager on X11
;;;
;;; Code:

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(exwm-config-ido)
(exwm-config-misc)

;; Share all windows across workspaces
(setq exwm-layout-show-all-buffers t
      exwm-workspace-show-all-buffers t)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(setq window-divider-default-right-width 1)
(window-divider-mode)

;; Auto detect multiple screens
(defvar exwm-xrandr-displays
  (split-string (shell-command-to-string "xrandr | grep ' connected' | awk '{ print $1 }'")))

(when (>= (length exwm-xrandr-displays) 2)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist (-flatten (-zip-with #'list '(0 1) exwm-xrandr-displays)))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr"
               nil
               (concat "xrandr --output " (nth 0 exwm-xrandr-displays) " --left-of " (nth 1 exwm-xrandr-displays) " --auto"))))
  (exwm-randr-enable))

(provide 'window-manager)
;;; window-manager.el ends here
