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

;; Better behavior for M-<backspace>
(global-set-key (kbd "M-<backspace>") 'kill-whitespace-or-word)

;; Better search
(global-set-key (kbd "C-s") 'swiper-isearch)

;; Multi-term
(global-set-key (kbd "C-x C-t") 'multi-term)
(global-set-key (kbd "C-t") (lambda()
                              (interactive)
                              (multi-term-dedicated-toggle)
                              (multi-term-dedicated-select)))

;; Multiple cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

;; Open remote file/sudo via TRAMP
(define-key global-map (kbd "C-x C-r") 'helm-tramp)

;; Set up shortcut keys
(global-set-key (kbd "M-?") 'zeal-at-point)
(global-set-key (kbd "M-x") 'helm-smex)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-m") 'helm-circe)
(global-set-key (kbd "C-x C-o") 'helm-projectile-switch-project)
(global-set-key (kbd "C-x f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x F") 'helm-projectile-find-file-in-known-projects)
(global-set-key (kbd "C-x C-f") 'helm-projectile-ag)
(global-set-key (kbd "C-x C-x") 'helm-imenu)
(global-set-key (kbd "C-x x") 'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-x C-v") 'helm-flycheck)
(global-set-key (kbd "<f8>") 'treemacs)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(global-set-key (kbd "C-x C-g c") 'magit-commit)
(global-set-key (kbd "C-x C-g d") 'magit-diff-buffer-file)
(global-set-key (kbd "C-x C-g C-d") 'magit-diff-unstaged)
(global-set-key (kbd "C-x C-g C-c") 'magit-commit-amend)
(global-set-key (kbd "C-x C-g p") 'magit-push-current-to-pushremote)
(global-set-key (kbd "C-x C-g r") 'magit-rebase-onto-upstream)
(global-set-key (kbd "C-x C-g b") (lambda ()
                                  (interactive)
                                  (if (bound-and-true-p magit-blame-mode)
                                    (magit-blame-quit)
                                    (call-interactively 'magit-blame-addition))))
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

;; Redfine keys for modes
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map ">" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil))

(with-eval-after-load 'rspec-mode
  (define-key rspec-mode-map (kbd "C-c C-c") 'rspec-verify-single)
  (define-key rspec-mode-map (kbd "C-c c") 'rspec-verify-all)
  (define-key rspec-mode-map (kbd "C-c C-f") 'rspec-run-last-failed)
  (define-key rspec-mode-map (kbd "C-c f") 'rspec-rerun))

(provide 'keys)
;;; keys.el ends here
