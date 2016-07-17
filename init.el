;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stewart Park's emacs init.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package setup
(setq package-archives '(
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
))
(setq package-list '(
    python-mode ruby-mode markdown-mode yaml-mode haskell-mode antlr-mode
    dockerfile-mode nasm-mode
    git-gutter magit
    org org-present org-trello
    hackernews
    ack fiplr
    neotree
    monokai-theme
))

;; Install and refresh the packages
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Configuration
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Happy Hacking!")

(setq make-backup-files nil)

(tool-bar-mode 0)
(menu-bar-mode 0)
(global-git-gutter-mode 1)
(global-linum-mode 1)

;; Theme
(load-theme 'monokai t)

;; Font setup
(set-face-attribute
    'default nil
    :family "Roboto Mono"
    :height 130
    :weight 'normal)

;; Neotree
(setq neo-smart-open t)
(neotree)

;; Set up shortcut keys
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(global-set-key (kbd "C-x C-f") 'ack)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-c t i") 'org-trello-install-board-metadata)
(global-set-key (kbd "C-c t c") 'org-trello-create-board-and-install-metadata)
(global-set-key (kbd "C-c t S-s") 'org-trello-sync-buffer)
(global-set-key (kbd "C-c t s") 'org-trello-sync-card)
(global-set-key (kbd "C-c t a") 'org-trello-add-card-comment)

;; Environment variable setup
(if (not (getenv "TERM_PROGRAM"))
  (let ((path (shell-command-to-string
          "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
    (setenv "PATH" path)))

;; Autorun
(add-hook 'after-init-hook (lambda ()

))

;; Hooks
(add-hook 'before-save-hook (lambda ()
    (delete-trailing-whitespace)
))
(add-hook 'org-present-mode-hook (lambda ()
    (neotree-hide)
    (org-present-big)
    (linum-mode 0)
    (git-gutter-mode 0)
    (org-display-inline-images)
    (set-frame-parameter nil 'fullscreen 'fullboth)
))
(add-hook 'org-present-mode-quit-hook (lambda ()
    (neotree-show)
    (other-window 1)
    (org-present-small)
    (linum-mode 1)
    (git-gutter-mode 1)
    (org-remove-inline-images)
    (set-frame-parameter nil 'fullscreen nil)
))

;; Org-mode babel config
(custom-set-variables
 '(org-babel-load-languages (quote (
    (emacs-lisp . t) (python . t) (dot . t) (ruby . t)
  )))
 '(org-confirm-babel-evaluate nil))

;; Mode setup for file extensions
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
