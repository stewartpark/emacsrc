;;; hooks.el --- Hooks defined
;;;
;;; Commentary:
;;; Hooks for different modes are defined here.
;;;
;;; Code:

;;; Variable used to store the original state before presenting
(defvar mode-line-format-backup nil)

;; Hooks
;; Don't ask about running processes
(add-hook 'comint-exec-hook (lambda ()
                              (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(add-hook 'before-save-hook (lambda ()
                              (delete-trailing-whitespace)
                              (when (bound-and-true-p python-mode)
                                (message "Python: isorting")
                                (py-isort-buffer))))

(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

(add-hook 'org-mode-hook (lambda ()
                           (emojify-mode)))

(add-hook 'org-present-mode-hook (lambda ()
                                   (setq word-wrap t)
                                   (setq mode-line-format-backup mode-line-format)
                                   (setq mode-line-format nil)
                                   (save-treemacs-state)
                                   (treemacs-hide)
                                   (zoom-window-zoom)
                                   (org-present-big)
                                   (git-gutter+-mode 0)
                                   (org-display-inline-images)
                                   (org-present-hide-cursor)
                                   (org-present-read-only)
                                   (save-fullscreen-state)
                                   (enter-fullscreen)))

(add-hook 'org-present-mode-quit-hook (lambda ()
                                        (setq word-wrap nil)
                                        (setq mode-line-format mode-line-format-backup)
                                        (setq mode-line-format-backup nil)
                                        (zoom-window-zoom)
                                        (restore-treemacs-state)
                                        (other-window 1)
                                        (org-present-small)
                                        (git-gutter+-mode 1)
                                        (org-remove-inline-images)
                                        (org-present-show-cursor)
                                        (org-present-read-write)
                                        (restore-fullscreen-state)))

(add-hook 'python-mode-hook (lambda ()
                              (eldoc-mode 0)
                              (py-autopep8-enable-on-save)
                              (pyenv-mode)))

(add-hook 'ruby-mode-hook (lambda ()
                            (robe-mode)
                            (inf-ruby-minor-mode)))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)

(add-hook 'typescript-mode-hook (lambda ()
                                  (tide-setup)
                                  (flycheck-mode +1)
                                  (setq flycheck-check-syntax-automatically '(save mode-enabled))
                                  (eldoc-mode +1)
                                  (tide-hl-identifier-mode +1)
                                  (company-mode +1)))

(add-hook 'yaml-mode-hook (lambda ()
                            (when (file-exists-p (concat (projectile-project-root) "ansible.cfg"))
                                (ansible 1))))

(add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)

(add-hook 'term-mode-hook #'eterm-256color-mode)

(add-hook 'prog-mode-hook (lambda ()
                            (require 'smartparens-config)
                            (sp-pair "'" nil :unless '(sp-point-after-word-p))
                            (sp-pair "%" "%" :wrap "C-%")
                            (sp-pair "<" ">" :wrap "C->")
                            (editorconfig-mode 1)
                            (company-mode)
                            (lsp-mode 1)
                            (lsp)
                            (git-gutter+-mode 1)
                            (display-line-numbers-mode 0)
                            (auto-revert-mode t)
                            (flycheck-mode 1)
                            (rainbow-mode 1)
                            (smartparens-mode 1)
                            (rainbow-delimiters-mode 1)
                            (dumb-jump-mode)))

(provide 'hooks)
;;; hooks.el ends here
