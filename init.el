;;; init.el --- Emacs boot up code
;;;
;;; Commentary:
;;; Stewart Park's Emacs configuration.
;;;
;;; Code:

;; Package setup
(setq package-archives '(
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
))
(setq package-list '(
    better-defaults multiple-cursors
    python-mode elpy py-isort py-autopep8 pyenv-mode-auto
    ruby-mode rspec-mode rbenv rubocop
    racket-mode
    markdown-mode yaml-mode haskell-mode antlr-mode
    dockerfile-mode nasm-mode go-mode foreman-mode js3-mode json-mode
    scss-mode web-mode rainbow-mode rainbow-delimiters
    smartparens dumb-jump
    git-gutter magit keychain-environment
    org org-present org-trello
    hackernews
    ack fiplr ace-window
    all-the-icons flycheck
    neotree
    molokai-theme
))

; For my own code
(load "~/.emacs.d/lisp/utils.el")
(global-set-key (kbd "M-=") 'font+)
(global-set-key (kbd "M--") 'font-)

;; Before anything starts, get the right envs
(when (not (getenv "TERM_PROGRAM"))
  (setenv "PATH" (shell-command-to-string "cat /etc/paths | tr '\n' ':'"))
  (setq exec-path (split-string (getenv "PATH") ":")))

;; Install and refresh the packages
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Configuration
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message (concat ";; Happy Hacking!\n;;\n" (get-all-documentations-as-comments)))
(setq-default cursor-type 'bar)

(setq make-backup-files nil)

(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
    (scroll-bar-mode 0))

;; Mac-specific config
(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq mac-allow-anti-aliasing t)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; Load keychain env
(keychain-refresh-environment)

;; Theme
(load-theme 'molokai t)

;; Neotree
(setq neo-smart-open t)
(setq neo-theme (if window-system 'icons 'arrow))
(setq neo-window-width 30)
(add-hook 'neotree-mode-hook (lambda () (setq cursor-type nil) (hl-line-mode 1)))
(neotree)

;; fiplr
(setq fiplr-ignored-globs
      '((directories (".git" ".svn" ".hg" ".bzr" ".bundle" "__pycache__"))
        (files (".DS_Store" "*.pyc" ".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip"))))

;; Multiple cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

;; Set up shortcut keys
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(global-set-key (kbd "C-x C-f") 'ack)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-c t i") 'org-trello-install-board-metadata)
(global-set-key (kbd "C-c t c") 'org-trello-create-board-and-install-metadata)
(global-set-key (kbd "C-c t S") 'org-trello-sync-buffer)
(global-set-key (kbd "C-c t s") 'org-trello-sync-card)
(global-set-key (kbd "C-c t a") 'org-trello-add-card-comment)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-d") 'dumb-jump-go)
(global-set-key (kbd "C-x d") 'dumb-jump-back)

;; Environment variable setup
(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)))

;; Autorun
;; NOTE: Please avoid adding anything here.
(add-hook 'after-init-hook (lambda ()))

;; Hooks
(add-hook 'before-save-hook (lambda ()
                              (delete-trailing-whitespace)
                              (when (bound-and-true-p python-mode)
                                (message "Python: isorting")
                                (py-isort-buffer)
                                )))
(add-hook 'org-present-mode-hook (lambda ()
                                   (neotree-hide)
                                   (org-present-big)
                                   (git-gutter-mode 0)
                                   (linum-mode 0)
                                   (org-display-inline-images)
                                   (set-frame-parameter nil 'fullscreen 'fullboth)))
(add-hook 'org-present-mode-quit-hook (lambda ()
                                        (neotree-show)
                                        (other-window 1)
                                        (org-present-small)
                                        (git-gutter-mode 1)
                                        (linum-mode 1)
                                        (org-remove-inline-images)
                                        (set-frame-parameter nil 'fullscreen nil)))

(add-hook 'python-mode-hook (lambda()
                              (py-autopep8-enable-on-save)
                              (pyenv-mode)
                              (elpy-mode)))

(add-hook 'ruby-mode-hook (lambda ()
                            (rubocop-mode)))

(add-hook 'prog-mode-hook (lambda ()
                            (git-gutter-mode 1)
                            (linum-mode 1)
                            (auto-revert-mode t)
                            (flycheck-mode 1)
                            (rainbow-mode)
                            (smartparens-mode)
                            (rainbow-delimiters-mode)
                            (dumb-jump-mode)))

(custom-set-faces
 ;; Default font
 '(default nil ((t (:family "Roboto Mono" :height 130 :weight 'light))))

 ;; Line number
 '(linum ((t (:height 120 :foreground "#C0C0C0"))))

 ;; NeoTree
 '(neo-dir-link-face ((t (:foreground "#C0C0C0"))))

 ;; Rainbow-delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "grey"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "white")))))

(custom-set-variables
 ;; Web mode related config
 '(web-mode-markup-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-code-indent-offset 2)

 ;; Ruby related config
 '(rbenv-show-active-ruby-in-modeline nil)
 '(ruby-insert-encoding-magic-comment nil)
 '(ruby-indent-level 2)

 ;; js3-mode related config
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

 ;; Line number format
 '(linum-format (quote "%5d"))

 ;; Org-babel
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ruby . t))))
 '(org-confirm-babel-evaluate nil))

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
(add-to-list 'auto-mode-alist '("Procfile\\'" . foreman))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
