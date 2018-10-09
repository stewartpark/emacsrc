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

;;Make non-floating windows resizable
(setq window-divider-default-right-width 2
      window-divider-default-right-width 2)
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

(desktop-environment-mode)
(desktop-environment-exwm-set-global-keybindings)

;; Application shortcuts
(exwm-input-set-key (kbd "s-s w") (lambda ()
                                  (interactive)
                                  (call-process-shell-command "google-chrome &" nil 0)))

(exwm-input-set-key (kbd "s-s f") (lambda ()
                                  (interactive)
                                  (call-process-shell-command "nautilus -w &" nil 0)))

(exwm-input-set-key (kbd "s-s S") (lambda ()
                                  (interactive)
                                  (call-process-shell-command "gnome-screenshot -a &" nil 0)))

(exwm-input-set-key (kbd "s-s s") (lambda ()
                                  (interactive)
                                  (call-process-shell-command "slack &" nil 0)))

;; Mac-like shortcuts
(exwm-input-set-key (kbd "s-c") (lambda ()
                                  (interactive)
                                  (exwm-input-send-next-key (kbd "C-c"))))

(exwm-input-set-key (kbd "s-v") (lambda ()
                                  (interactive)
                                  (exwm-input-send-next-key (kbd "C-v"))))

(exwm-input-set-key (kbd "s-w") (lambda ()
                                  (interactive)
                                  (exwm-input-send-next-key (kbd "C-w"))))

;; Simulate Emacs shortcuts on applications.
(setq exwm-input-simulation-keys
      '(
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])

        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])

        ;; search
        ([?\C-s] . [?\C-f])))

;; Start EXWM
(exwm-enable)

(provide 'window-manager)
;;; window-manager.el ends here
