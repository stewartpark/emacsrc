;;; window-manager.el --- Make Emacs act as a window manager
;;;
;;; Commentary:
;;; Use exwm to make Emacs act as a window manager on X11
;;;
;;; Code:

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(setq window-divider-default-right-width 1)
(window-divider-mode)

(provide 'window-manager)
;;; window-manager.el ends here
