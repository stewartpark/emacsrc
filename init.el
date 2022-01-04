(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(setq
  custom-file (concat user-emacs-directory "/custom.el")

  projectile-project-search-path '("~/Workspace/")

  make-backup-files nil
  
  inhibit-splash-screen t
  inhibit-startup-message t
  initial-scratch-message ";; Happy Hacking!")

(custom-set-variables
 '(tab-width 4 't))

(global-linum-mode t)
(xterm-mouse-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-time-mode t)
(when (display-graphic-p)
  ;; Font setup
  (let ((font-face "Ubuntu Mono-15"))
    (set-face-attribute 'default nil :font font-face)
    (set-face-attribute 'variable-pitch nil :font font-face)
    (set-frame-font font-face nil t)
    (setq default-frame-alist '((font . "Ubuntu Mono-15"))))

  (when (eq system-type 'gnu/linux)
    (set-frame-parameter nil 'undecorated t))
  (scroll-bar-mode 0))

;; Global Keys
(global-set-key (kbd "M-<return>") 'toggle-frame-fullscreen)

;; Major Modes
(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package ruby-mode)

(use-package python)

(use-package go-mode)

(use-package rust-mode)

(use-package svelte-mode
  :mode ("\\.svelte\\'" . svelte-mode))

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package json-mode)

(use-package yaml-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
		 (ruby-mode . lsp)
		 (go-mode . lsp)
		 (rust-mode . lsp)
		 (typescript-mode . lsp)
		 (js-mode . lsp)
		 (svelte-mode . lsp)
		 (terraform-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; Minor Modes & Utilities
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-S-s") 'counsel-rg)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x f") 'counsel-git)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer))

(use-package counsel-projectile
  :ensure t
  :config
  (global-set-key (kbd "C-x C-o") 'counsel-projectile-switch-project))

(use-package counsel-tramp
  :ensure t
  :config
  (global-set-key (kbd "C-x C-t") 'counsel-tramp))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("M-s ." . swiper-isearch-thing-at-point))
  :config
  (setq swiper-action-recenter t
        swiper-goto-start-of-match t))

(use-package projectile
  :ensure t)

(use-package company
  :init
  (setq company-async-timeout 15
        company-tooltip-align-annotations t)
  :hook (after-init . global-company-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g c" . magit-commit)
		 ("C-x C-g b" . (lambda ()
                          (interactive)
                          (if (bound-and-true-p magit-blame-mode)
                              (magit-blame-quit)
                            (call-interactively 'magit-blame-addition))))))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook 'prog-mode
  :config
  :diminish 'rainbow-mode)

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package pinentry
  :ensure t
  :init
  (pinentry-start))

(use-package docker-tramp)

(use-package kubernetes-tramp)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
		doom-themes-treemacs-theme "doom-atom")
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-lsp t
		doom-modeline-icon nil))
