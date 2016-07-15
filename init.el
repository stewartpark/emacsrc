; Stewart Park's emacs init.el

; Package setup
(setq package-archives '(
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
))
(setq package-list '(
    python-mode ruby-mode markdown-mode yaml-mode haskell-mode antlr-mode
    dockerfile-mode
    git-gutter magit
    org org-present org-trello
    ack fiplr
    neotree
    monokai-theme
))
(package-initialize)

; Misc. setup
(if (not (getenv "TERM_PROGRAM"))
  (let ((path (shell-command-to-string
          "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
    (setenv "PATH" path)))
(setq make-backup-files nil)

; Install and refresh the packages
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Autorun
(add-hook 'after-init-hook (lambda ()
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (global-git-gutter-mode 1)
    (global-linum-mode 1)

    ; Theme
    (load-theme 'monokai t)

    ; Launch neotree
    (neotree)

    ; Set up keys
    (global-set-key (kbd "C-x f") 'fiplr-find-file)
    (global-set-key (kbd "C-x C-f") 'ack)
    (global-set-key (kbd "C-x e") 'neotree-toggle)
))

; Hooks
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
    (org-present-small)
    (linum-mode 1)
    (git-gutter-mode 1)
    (org-remove-inline-images)
))

(set-face-attribute
    'default nil
    :family "Roboto Mono"
    :height 150
    :weight 'normal)

(custom-set-variables
 '(org-babel-load-languages (quote (
    (emacs-lisp . t) (python . t) (dot . t) (ruby . t)
  )))
 '(org-confirm-babel-evaluate nil))
