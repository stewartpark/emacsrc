;;; init.el --- Emacs boot up code
;;;
;;; Commentary:
;;; Stewart Park's Emacs configuration.
;;;
;;; Code:

;;; TODO: use use-package, smart-jump

;; Package setup
(setq package-archives '(
    ("melpa" . "https://melpa.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
))
(setq package-list '(
    better-defaults multiple-cursors
    python-mode anaconda-mode py-isort py-autopep8 pyenv-mode-auto pythonic
    ruby-mode rspec-mode rbenv inf-ruby robe bundler crystal-mode
    racket-mode elm-mode rust-mode cargo vue-mode rjsx-mode typescript-mode
    markdown-mode yaml-mode haskell-mode antlr-mode groovy-mode
    scala-mode sbt-mode ensime
    dockerfile-mode nasm-mode go-mode foreman-mode js2-mode json-mode
    ansible scss-mode web-mode rainbow-mode rainbow-delimiters
    smartparens dumb-jump zoom-window dotenv-mode
    fringe-helper git-gutter-fringe+ magit keychain-environment
    org org-present
    hackernews transpose-frame
    all-the-icons
    flycheck flycheck-rust flycheck-crystal flycheck-popup-tip
    company company-racer racer company-restclient
    doom-themes restart-emacs cov
    helm-tramp docker-tramp tramp-hdfs
    ace-window multi-term nlinum doom-modeline
    projectile treemacs treemacs-projectile ag editorconfig
    helm helm-projectile helm-ag helm-circe helm-company helm-spotify helm-flycheck swiper-helm
    emojify circe circe-notifications json json-rpc restclient zeal-at-point symon
))

;; Install and refresh the packages
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; For my own code
(load "~/.emacs.d/lisp/utils.el")
(load "~/.emacs.d/lisp/resque-mode.el")
(load "~/.emacs.d/lisp/symon-monitors.el")

;;; Theme-related
;; Font setup
(let ((font-face "Inconsolata:pixelsize=18"))
  (set-face-attribute 'default nil :font font-face)
  (set-face-attribute 'variable-pitch nil :font font-face)
  (set-frame-font font-face nil t)
  (set-fontset-font "fontset-default" '(#xac00 . #xd7a3) "NanumGothicCoding"))

;; Load theme
(load-random-theme
 '(doom-nord-light))

;; Related modes
(global-visual-line-mode 1)
(doom-modeline-init)
(doom-themes-org-config)
(doom-themes-treemacs-config)
(doom-themes-visual-bell-config)
(xterm-mouse-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
  (set-frame-parameter nil 'undecorated t)
  (scroll-bar-mode 0))

;;;; Configuration
(setq-default
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message (concat ";; Happy Hacking!\n;;\n" (get-all-documentations-as-comments))
 cursor-type 'bar
 indent-tabs-mode nil
 make-backup-files nil
 create-lockfiles nil
 vc-follow-symlinks t

 ;; Tramp
 tramp-default-method "ssh"

 ;; Python
 python-shell-prompt-detect-failure-warning nil
 python-shell-completion-native-disabled-interpreters (list "python" "pypy")

 ;; Ruby
 inf-ruby-default-implementation "pry"
 inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *"
 inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *"
 inf-ruby-console-environment "development"

 ;; Web-mode
 web-mode-enable-auto-pairing nil

 ;; Truncation
 truncate-partial-width-windows nil
 truncate-lines t

 ;; Org-mode
 org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))
 org-image-actual-width nil

 ;; Helm
 helm-display-header-line nil
 helm-autoresize-max-height 10
 helm-autoresize-min-height 10
 helm-split-window-in-side-p t

 ;; Ace window
 aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)

 ;; Treemacs
 treemacs-collapse-dirs 5
 treemacs-space-between-root-nodes nil
 treemacs-git-mode 'extended
 treemacs-project-follow-cleanup t

 ;; Flycheck
 flycheck-scala-scalastyle-executable "scalastyle"
 flycheck-scalastylerc "scalastyle-config.xml"

 ;; Ensime
 ensime-startup-notification nil

 ;; Rspec
 rspec-use-spring-when-possible nil
 compilation-scroll-output t

 ;; Projectile
 projectile-project-search-path '("~/Workspace")

 ;; Ansible
 ansible::vault-password-file "~/.vault-pass"
)

;; Treemacs actions
(with-eval-after-load 'treemacs
  (treemacs-follow-mode)
  (treemacs-filewatch-mode)
  (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace))

;;;; Adhoc fixes
;; Ignore yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
;; Do not save packages in init.el
(defun package--save-selected-packages (&rest opt) "OPT." nil)

;; Line number font size fix
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;;;; Global shortcut
;; Font resizing
(global-set-key (kbd "M-=") 'font+)
(global-set-key (kbd "M--") 'font-)

;; Scrolling
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-6>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-7>") 'scroll-up-line)

;; Better behavior for M-<backspace>
(global-set-key (kbd "M-<backspace>") 'kill-whitespace-or-word)


;; Better search
(global-set-key (kbd "C-s") 'swiper)

;; Multi-term
(global-set-key (kbd "C-x C-t") 'multi-term)

;; Multiple cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

;; Open remote file/sudo via TRAMP
(define-key global-map (kbd "C-x C-r") 'helm-tramp)

;; Set up shortcut keys
(global-set-key (kbd "M-?") 'zeal-at-point)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-m") 'helm-circe)
(global-set-key (kbd "C-x C-o") 'helm-projectile-switch-project)
(global-set-key (kbd "C-x f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-f") 'helm-projectile-ag)
(global-set-key (kbd "C-x C-x") 'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-x C-v") 'helm-flycheck)
(global-set-key (kbd "<f8>") 'treemacs)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-.") 'dumb-jump-go)
(global-set-key (kbd "M-,") 'dumb-jump-back)
(global-set-key (kbd "C-x C-g c") 'magit-commit)
(global-set-key (kbd "C-x C-g d") 'magit-diff-buffer-file)
(global-set-key (kbd "C-x C-g C-c") 'magit-commit-amend)
(global-set-key (kbd "C-x C-g p") 'magit-push-current-to-pushremote)
(global-set-key (kbd "C-x C-g r") 'magit-rebase-onto-upstream)
(global-set-key (kbd "C-x C-g b") (lambda ()
                                  (interactive)
                                  (if (bound-and-true-p magit-blame-mode)
                                    (magit-blame-quit)
                                    (call-interactively 'magit-blame))))
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


;; Autorun
;; NOTE: Please avoid adding anything here.
(add-hook 'after-init-hook (lambda ()))

;; Hooks

;; Don't ask about running processes
(add-hook 'comint-exec-hook (lambda ()
                              (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(add-hook 'before-save-hook (lambda ()
                              (delete-trailing-whitespace)
                              (when (bound-and-true-p python-mode)
                                (message "Python: isorting")
                                (py-isort-buffer)
                                )))

(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

(add-hook 'org-mode-hook (lambda ()
                           (emojify-mode)))

(add-hook 'org-present-mode-hook (lambda ()
                                   (setq word-wrap t)
                                   (setq mode-line-format-backup mode-line-format)
                                   (setq mode-line-format nil)
                                   (symon-mode nil)
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
                                        (symon-mode t)
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
                              (run-python (python-shell-parse-command))
                              (py-autopep8-enable-on-save)
                              (pyenv-mode)
                              (anaconda-mode)))

(add-hook 'ruby-mode-hook (lambda ()
                            (robe-mode)
                            (coverage-mode)
                            (inf-ruby-minor-mode)))

(add-hook 'rust-mode-hook #'racer-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'yaml-mode-hook (lambda ()
                            (when (file-exists-p (concat (projectile-project-root) "ansible.cfg"))
                                (ansible 1))))

(add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)

(add-hook 'prog-mode-hook (lambda ()
                            (require 'smartparens-config)
                            (sp-pair "'" nil :unless '(sp-point-after-word-p))
                            (sp-pair "%" "%" :wrap "C-%")
                            (sp-pair "<" ">" :wrap "C->")
                            (editorconfig-mode 1)
                            (company-mode)
                            (cov-mode)
                            (git-gutter+-mode 1)
                            (nlinum-mode 1)
                            (auto-revert-mode t)
                            (flycheck-mode 1)
                            (rainbow-mode)
                            (smartparens-mode)
                            (rainbow-delimiters-mode)
                            (dumb-jump-mode)))

;;;; Emacs-generated configuration
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#909090"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#D0D000"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "medium sea green")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(js-indent-level 2)
 '(js2-auto-indent-p t)
 '(js2-consistent-level-indent-inner-bracket t)
 '(js2-curly-indent-offset 0)
 '(js2-enter-indents-newline t)
 '(js2-expr-indent-offset 2)
 '(js2-indent-on-enter-key t)
 '(js2-lazy-commas t)
 '(js2-lazy-dots t)
 '(js2-lazy-operators t)
 '(js2-paren-indent-offset 2)
 '(js2-square-indent-offset 2)
 '(typescript-indent-level 2)
 '(nlinum-format (quote "%4d"))
 '(nlinum-highlight-current-line t)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
     (dot . t)
     (sql . t)
     (ruby . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-src-tab-acts-natively t)
 '(org-support-shift-select t)
 '(rbenv-show-active-ruby-in-modeline nil)
 '(ruby-align-to-stmt-keywords (quote (if begin case)))
 '(ruby-deep-indent-paren nil)
 '(ruby-indent-level 2)
 '(ruby-insert-encoding-magic-comment nil)
 '(tab-width 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))

;; Mode setup for file extensions
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))
(add-to-list 'auto-mode-alist '("Procfile\\'" . foreman))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.req" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.scala" . scala-mode))

;;;; Environment-specific configuration
;; Mac-specific config
(when (eq system-type 'darwin)
  ;; Before anything starts, get the right envs
  (when (not (getenv "TERM_PROGRAM"))
    (setenv "PATH" (shell-command-to-string "cat /etc/paths | tr '\n' ':'"))
    exec-path (split-string (getenv "PATH") ":"))
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

;; Environment variable setup
(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)
      (setq exec-path (split-string path ":"))))

;; Host-specific configurations
(if (file-exists-p "~/.config/emacs.el")
    (load "~/.config/emacs.el"))

;;;; Things to run at startup
;; Load keychain env
(keychain-refresh-environment)

;; Git-gutter-fringe
(when (display-graphic-p)
  (require 'git-gutter-fringe+))

;; Company-mode
(eval-after-load 'company
  '(push 'company-robe company-backends))

(with-eval-after-load 'flycheck
  (flycheck-popup-tip-mode))

(projectile-cleanup-known-projects)
(dolist (project-path (car (last (car (projectile-discover-projects-in-search-path)))))
  (projectile-add-known-project project-path))

(symon-mode)
(helm-projectile-on)
(server-start)

(provide 'init)
;;; init.el ends here
