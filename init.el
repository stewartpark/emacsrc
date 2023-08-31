(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
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
  vc-follow-symlinks nil

  mac-command-modifier 'meta
  mac-option-modifier 'super
  mac-control-modifier 'control
  ns-function-modifier 'hyper

  inhibit-splash-screen t
  inhibit-startup-message t
  initial-scratch-message ";; Happy Hacking!")

(custom-set-variables
 '(tab-width 4 't))

(xterm-mouse-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-time-mode t)
(when (display-graphic-p)
  (pixel-scroll-precision-mode t)

  ;; Font setup
  (let ((font-face "Ubuntu Mono-16"))
    (set-face-attribute 'default nil :font font-face)
    (set-face-attribute 'variable-pitch nil :font font-face)
    (set-frame-font font-face nil t)
    (setq default-frame-alist '((font . "Ubuntu Mono-16"))))

  (when (eq system-type 'gnu/linux)
    (set-frame-parameter nil 'undecorated t))
  (scroll-bar-mode 0))

(load "~/.emacs.d/lisp/utils.el")

(make-thread
 (server-start)
 (projectile-discover-projects-in-search-path))

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Global Keys
(global-set-key (kbd "M-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<backspace>") 'kill-whitespace-or-word-backward)

;; Major Modes
(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package ruby-mode
  :ensure t)

(use-package python
  :ensure t)

(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package rust-mode
  :ensure t)

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode))

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile" . dockerfile-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package restclient
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (("M-/" . company-complete))
  :hook ((python-mode . lsp)
		 (ruby-mode . lsp)
		 (go-mode . lsp)
		 (rust-mode . lsp)
		 (typescript-mode . lsp)
		 (js-mode . lsp)
		 (svelte-mode . lsp)
		 (terraform-mode . lsp)
		 (dockerfile-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; Minor Modes & Utilities
(use-package editorconfig
  :ensure t
  :hook (prog-mode . editorconfig-mode))

(use-package flx
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-S-s") 'counsel-rg)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x f") 'counsel-projectile-find-file)
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
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("M-s ." . swiper-isearch-thing-at-point))
  :config
  (setq swiper-action-recenter t
        swiper-goto-start-of-match t))

(use-package projectile
  :ensure t)

(use-package company
  :ensure t
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

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)

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

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
		doom-themes-treemacs-theme "doom-atom")
  (load-theme 'doom-moonlight t)
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
