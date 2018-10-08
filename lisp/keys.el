;;; keys.el --- Custom keys settings
;;;
;;; Commentary:
;;; Stewart Park's custom shortcuts are defined here
;;;
;;; Code:

;;;; Global shortcut
;; Font resizing
(global-set-key (kbd "M-=") 'font+)
(global-set-key (kbd "M--") 'font-)

;; Scrolling
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-6>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-7>") 'scroll-up-line)

;; Better behavior for M-<backspace>
(global-set-key (kbd "M-<backspace>") 'kill-whitespace-or-word)

;; Better search
(global-set-key (kbd "C-s") 'swiper)

;; Multi-term
(global-set-key (kbd "C-x C-t") 'multi-term)

;; Multiple cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

;; Open remote file/sudo via TRAMP
(define-key global-map (kbd "C-x C-r") 'helm-tramp)

;; Set up shortcut keys
(global-set-key (kbd "M-?") 'zeal-at-point)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-m") 'helm-circe)
(global-set-key (kbd "C-x C-o") 'helm-projectile-switch-project)
(global-set-key (kbd "C-x f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-f") 'helm-projectile-ag)
(global-set-key (kbd "C-x C-x") 'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-x C-v") 'helm-flycheck)
(global-set-key (kbd "<f8>") 'treemacs)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-.") 'dumb-jump-go)
(global-set-key (kbd "M-,") 'dumb-jump-back)
(global-set-key (kbd "C-x C-g c") 'magit-commit)
(global-set-key (kbd "C-x C-g d") 'magit-diff-buffer-file)
(global-set-key (kbd "C-x C-g C-c") 'magit-commit-amend)
(global-set-key (kbd "C-x C-g p") 'magit-push-current-to-pushremote)
(global-set-key (kbd "C-x C-g r") 'magit-rebase-onto-upstream)
(global-set-key (kbd "C-x C-g b") (lambda ()
                                  (interactive)
                                  (if (bound-and-true-p magit-blame-mode)
                                    (magit-blame-quit)
                                    (call-interactively 'magit-blame))))
(global-set-key (kbd "C-x C-g l") (lambda ()
                                (interactive)
                                (call-interactively 'magit-log-all-branches)))
(global-set-key (kbd "C-x SPC") (lambda ()
                                  (interactive)
                                  (save-treemacs-state)
                                  (treemacs-hide)
                                  (transpose-frame)
                                  (restore-treemacs-state)))
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
(global-set-key (kbd "C-x C-p") (lambda ()
                                  (interactive)
                                  (if (eq major-mode 'org-mode)
                                    (if (bound-and-true-p org-present-mode)
                                      (call-interactively 'org-present-quit)
                                      (call-interactively 'org-present))
                                    (message "Not in org-mode"))))
(global-set-key (kbd "s-<return>") (lambda ()
                                   (interactive)
                                   (if (fullscreen-p)
                                     (quit-fullscreen)
                                     (enter-fullscreen))))

(provide 'keys)
;;; keys.el ends here