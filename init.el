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
    better-defaults
    python-mode elpy py-isort py-autopep8 pyenv-mode-auto
    ruby-mode rspec-mode rbenv rubocop
    racket-mode
    markdown-mode yaml-mode haskell-mode antlr-mode
    dockerfile-mode nasm-mode go-mode foreman-mode js3-mode json-mode
    scss-mode web-mode
    git-gutter magit keychain-environment
    org org-present org-trello
    hackernews
    ack fiplr ace-window
    neotree flycheck
    dark-mint-theme
))

;; Before anything starts, get the right envs
(when (not (getenv "TERM_PROGRAM"))
  (setenv "PATH"
    (shell-command-to-string "cat /etc/paths | tr '\n' ':'"))
  (setq exec-path (split-string (getenv "PATH") ":"))
)

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

(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
    (scroll-bar-mode 0)
)
(when (eq system-type 'darwin)
    (defun copy-from-osx ()
        (shell-command-to-string "pbpaste"))
    (defun paste-to-osx (text &optional push)
        (let ((process-connection-type nil))
            (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
                (process-send-string proc text)
                (process-send-eof proc))))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)
)

(global-git-gutter-mode 1)
(global-linum-mode 0)
(global-auto-revert-mode t)
(global-flycheck-mode)
(setq rbenv-show-active-ruby-in-modeline nil)
(global-rbenv-mode)

;; Ruby-related config
(setq ruby-insert-encoding-magic-comment nil)
(setq ruby-indent-level 2)

;; Load keychain env
(keychain-refresh-environment)

;; Theme
(load-theme 'dark-mint t)

;; Font setup
(setq mac-allow-anti-aliasing t)
(set-face-attribute
    'default nil
    :family "Roboto Mono"
    :height 120
    :weight 'light)

;; Neotree
(setq neo-smart-open t)
(neotree)

;; Elpy
(elpy-enable)

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
    (when (bound-and-true-p python-mode)
        (message "Python: isorting")
        (py-isort-buffer)
    )
))
(add-hook 'org-present-mode-hook (lambda ()
    (neotree-hide)
    (org-present-big)
    (git-gutter-mode 0)
    (org-display-inline-images)
    (set-frame-parameter nil 'fullscreen 'fullboth)
))
(add-hook 'org-present-mode-quit-hook (lambda ()
    (neotree-show)
    (other-window 1)
    (org-present-small)
    (git-gutter-mode 1)
    (org-remove-inline-images)
    (set-frame-parameter nil 'fullscreen nil)
))
(add-hook 'web-mode-hook (lambda ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'ruby-mode-hook #'rubocop-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js3-auto-indent-p t)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-expr-indent-offset 2)
 '(js3-indent-on-enter-key t)
 '(js3-lazy-commas t)
 '(js3-lazy-dots t)
 '(js3-lazy-operators t)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ruby . t))))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (dark-mint-theme yaml-mode web-mode scss-mode rubocop rspec-mode rbenv racket-mode python-mode pyenv-mode-auto py-isort py-autopep8 org-trello org-present neotree nasm-mode monochrome-theme material-theme markdown-mode magit keychain-environment json-mode js3-mode haskell-mode hackernews go-mode git-gutter foreman-mode flycheck flatland-theme fiplr elpy dockerfile-mode better-defaults ack ace-window))))

;; Mode setup for file extensions
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
