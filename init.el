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
    ruby-mode rspec-mode rbenv rubocop rust-mode
    racket-mode elm-mode rust-mode vue-mode rjsx-mode typescript-mode
    markdown-mode yaml-mode haskell-mode antlr-mode groovy-mode
    dockerfile-mode nasm-mode go-mode foreman-mode js3-mode json-mode
    scss-mode web-mode rainbow-mode rainbow-delimiters
    smartparens dumb-jump zoom-window inf-ruby
    fringe-helper git-gutter-fringe+ magit keychain-environment
    org org-present adaptive-wrap diminish
    hackernews transpose-frame
    ag fiplr ace-window
    all-the-icons flycheck
    neotree pivotal-tracker
    zweilight-theme
    emojify
    json json-rpc
))

;; For my own code
(load "~/.emacs.d/lisp/utils.el")
(global-set-key (kbd "M-=") 'font+)
(global-set-key (kbd "M--") 'font-)

;; Host-specific configurations
(if (file-exists-p "~/.config/pivotal-tracker.key")
    (setq pivotal-api-token (replace-regexp-in-string "\n$" "" (file:read "~/.config/pivotal-tracker.key"))))

;; Install and refresh the packages
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Configuration
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message
      (concat ";; Happy Hacking!\n;;\n" (get-all-documentations-as-comments)))
(setq-default cursor-type 'bar)
(setq vc-follow-symlinks t)
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-completion-native-disabled-interpreters (list "python" "pypy"))
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

(require 'smartparens-config)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions :rem)

(setq truncate-partial-width-windows nil)
(setq truncate-lines t)
(global-visual-line-mode 1)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))

;; Don't ask about running processes
(add-hook 'comint-exec-hook (lambda ()
                              (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(setq make-backup-files nil)

(let ((font-face "Hack-12"))
  (set-face-attribute 'default nil :font font-face)
  (set-frame-font font-face nil t))

(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
    (scroll-bar-mode 0))

;; Mac-specific config
(when (eq system-type 'darwin)
  ;; Before anything starts, get the right envs
  (when (not (getenv "TERM_PROGRAM"))
    (setenv "PATH" (shell-command-to-string "cat /etc/paths | tr '\n' ':'"))
    (setq exec-path (split-string (getenv "PATH") ":")))
  ;; Mac utils
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
(if (display-graphic-p)
  (load-theme 'zweilight t)
  (load-theme 'tango-dark t))

;; Git-gutter-fringe
(when (display-graphic-p)
  (require 'git-gutter-fringe+))

;; Ace window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Neotree
(with-eval-after-load 'neotree
  (define-key neotree-mode-map (kbd "RET") (neotree-make-executor :file-fn 'neo-open-file-ace-window :dir-fn 'neo-open-dir)))
(setq neo-smart-open t)
(setq neo-theme (if window-system 'icons 'arrow))
(setq neo-window-width 30)
(add-hook 'neotree-mode-hook (lambda ()
                               (setq cursor-type nil)
                               (toggle-truncate-lines 1)
                               (hl-line-mode 1)
                               (visual-line-mode 0)
                               (adaptive-wrap-prefix-mode 0)))
(neotree)

;; fiplr
(setq fiplr-ignored-globs
      '((directories (".git" ".svn" ".hg" ".bzr" ".bundle" "__pycache__" "node_modules" "bower_components"))
        (files (".DS_Store" "*.pyc" ".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip"))))

;; Line number font size fix
(defun linum-update-window-scale-fix (win)
  (set-window-margins
   win
   (ceiling (* (if (boundp 'text-scale-mode-step)
                   (expt text-scale-mode-step
                         text-scale-mode-amount) 1)
               (if (car (window-margins))
                   (car (window-margins)) 1)
               ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;; Multiple cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

;; Set up shortcut keys
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(global-set-key (kbd "C-x C-f") 'ag-project)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-d") 'dumb-jump-go)
(global-set-key (kbd "C-x d") 'dumb-jump-back)
(global-set-key (kbd "C-x C-g") (lambda ()
                                  (interactive)
                                  (if (bound-and-true-p magit-blame-mode)
                                    (magit-blame-quit)
                                    (call-interactively 'magit-blame))))
(global-set-key (kbd "C-x SPC") (lambda ()
                                  (interactive)
                                  (save-neotree-state)
                                  (neotree-hide)
                                  (transpose-frame)
                                  (restore-neotree-state)))
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

;; Environment variable setup
(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)
      (setq exec-path (split-string path ":"))))

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

(add-hook 'visual-line-mode-hook
  (lambda ()
    (adaptive-wrap-prefix-mode 1)
    (diminish 'visual-line-mode)))

(add-hook 'org-mode-hook (lambda ()
                           (emojify-mode)))

(add-hook 'org-present-mode-hook (lambda ()
                                   (setq word-wrap t)
                                   (save-neotree-state)
                                   (neotree-hide)
                                   (org-present-big)
                                   (git-gutter+-mode 0)
                                   (org-display-inline-images)
                                   (save-fullscreen-state)
                                   (enter-fullscreen)))

(add-hook 'org-present-mode-quit-hook (lambda ()
                                        (setq word-wrap nil)
                                        (restore-neotree-state)
                                        (other-window 1)
                                        (org-present-small)
                                        (git-gutter+-mode 1)
                                        (org-remove-inline-images)
                                        (restore-fullscreen-state)))

(add-hook 'python-mode-hook (lambda ()
                              (run-python (python-shell-parse-command))
                              (py-autopep8-enable-on-save)
                              (pyenv-mode)
                              (elpy-mode)))

(add-hook 'ruby-mode-hook (lambda ()
                            (rubocop-mode)
                            (inf-ruby-minor-mode)))

(add-hook 'js3-mode-hook (lambda ()
                           (linum-mode)))

(add-hook 'prog-mode-hook (lambda ()
                            (git-gutter+-mode 1)
                            (linum-mode 1)
                            (auto-revert-mode t)
                            (flycheck-mode 1)
                            (rainbow-mode)
                            (smartparens-mode)
                            (rainbow-delimiters-mode)
                            (dumb-jump-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "gray"))))
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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(js3-auto-indent-p t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-expr-indent-offset 2)
 '(js3-indent-on-enter-key t)
 '(js3-lazy-commas t)
 '(js3-lazy-dots t)
 '(js3-lazy-operators t)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(linum-format (quote "%3d"))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
     (dot . t)
     (sql . t)
     (ruby . t))))
 '(org-confirm-babel-evaluate nil)
 '(rbenv-show-active-ruby-in-modeline nil)
 '(ruby-align-to-stmt-keywords (quote (if begin case)))
 '(ruby-deep-indent-paren nil)
 '(ruby-indent-level 2)
 '(ruby-insert-encoding-magic-comment nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))

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
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
