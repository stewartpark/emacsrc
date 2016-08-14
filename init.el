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
    dockerfile-mode nasm-mode go-mode foreman-mode js3-mode json-mode
    scss-mode web-mode
    git-gutter magit
    org org-present org-trello
    hackernews
    ack fiplr ace-window
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

(setq ruby-insert-encoding-magic-comment nil)

(setq make-backup-files nil)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-git-gutter-mode 1)
(global-linum-mode 1)
(global-auto-revert-mode t)

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
(global-set-key (kbd "C-c t S") 'org-trello-sync-buffer)
(global-set-key (kbd "C-c t s") 'org-trello-sync-card)
(global-set-key (kbd "C-c t a") 'org-trello-add-card-comment)
(global-set-key (kbd "M-p") 'ace-window)

;; Environment variable setup
(if (not (getenv "TERM_PROGRAM"))
  (let ((path (shell-command-to-string
          "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
    (setenv "PATH" path)))

;; Autorun
(add-hook 'after-init-hook (lambda ()
  (define-key neotree-mode-map (kbd "i") #'neotree-enter-horizontal-split)
  (define-key neotree-mode-map (kbd "I") #'neotree-enter-vertical-split)
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
(add-hook 'web-mode-hook (lambda ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
))

(custom-set-variables
  ;; js3-mode config
  '(js3-lazy-commas t)
  '(js3-lazy-operators t)
  '(js3-expr-indent-offset 2)
  '(js3-paren-indent-offset 2)
  '(js3-square-indent-offset 2)
  '(js3-curly-indent-offset 2)

;; Org-mode babel config
  '(org-babel-load-languages
    (quote
      ((emacs-lisp . t)
        (python . t)
        (dot . t)
        (ruby . t))))
  '(org-confirm-babel-evaluate nil)
)

;; Mode setup for file extensions
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
