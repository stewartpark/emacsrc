;;; config.el --- Emacs configuration
;;;
;;; Commentary:
;;; Anything related to variables, themes, and faces.
;;;
;;; Code:

;; Load theme
(load-theme 'doom-henna t)

(xterm-mouse-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-time-mode t)

(when (display-graphic-p)
  ;; Font setup
  (let ((font-face "Inconsolata-12"))
    (set-face-attribute 'default nil :font font-face)
    (set-face-attribute 'variable-pitch nil :font font-face)
    (set-frame-font font-face nil t)
    (setq default-frame-alist '((font . "Inconsolata-12")))
    (set-fontset-font "fontset-default" '(#xac00 . #xd7a3) "NanumGothicCoding-12"))

  (when (eq system-type 'gnu/linux)
    (set-frame-parameter nil 'undecorated t))
  (scroll-bar-mode 0))

;; Global mode settings
(make-thread
 (lambda ()
   (global-magit-file-mode 1)
   (global-rbenv-mode 1)))

;;;; Adhoc fixes
;; Ignore yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
;; Do not save packages in init.el
(defun package--save-selected-packages (&rest opt) "OPT." nil)

;;;; Configuration
(setq-default
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message (concat ";; Happy Hacking!\n;;\n" (get-all-documentations-as-comments))
 cursor-type 'bar
 indent-tabs-mode nil
 column-number-mode t
 line-number-mode t
 make-backup-files nil
 create-lockfiles nil
 vc-follow-symlinks t
 auto-window-vscroll nil
 epa-pinentry-mode 'loopback
 require-final-newline t
 custom-file (concat user-emacs-directory "/lisp/config.el")

 split-height-threshold nil
 split-width-threshold 0

 ;; Scroll optimization
 mouse-wheel-scroll-amount '(1)
 mouse-wheel-follow-mouse t
 mouse-wheel-progressive-speed nil
 fast-but-imprecise-scrolling t
 inhibit-compacting-font-caches t
 scroll-margin 0
 scroll-conservatively 0
 scroll-step 1
 scroll-preserve-screen-position t
 scroll-error-top-bottom t
 redisplay-dont-pause t
 next-error-recenter '(4)
 jit-lock-defer-time 0
 ring-bell-function 'ignore

 ;; Tramp
 tramp-default-method "ssh"

 ;; LSP
 lsp-restart 'auto-restart
 lsp-python-ms-auto-install-server t

 ;; Python
 python-shell-prompt-detect-failure-warning nil
 python-shell-completion-native-disabled-interpreters (list "python" "pypy")

 ;; Ruby
 inf-ruby-default-implementation "pry"
 inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *"
 inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *"
 inf-ruby-console-environment "development"

 ;; Rust
 rust-format-on-save t

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
 helm-display-header-line nil
 helm-echo-input-in-header-line nil
 helm-swoop-split-with-multiple-windows nil
 helm-swoop-split-direction 'split-window-vertically
 helm-swoop-split-window-function 'helm-default-display-buffer

 ;; Ace window
 aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)

 ;; Treemacs
 treemacs-collapse-dirs 5
 treemacs-space-between-root-nodes nil
 treemacs-git-mode 'deferred
 treemacs-project-follow-cleanup t
 treemacs-wrap-around t
 treemacs-indentation 1
 treemacs-display-in-side-window t
 doom-themes-treemacs-theme "doom-colors"

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

;; Ace window
(ace-window-display-mode t)

;; Doom themes
(doom-modeline-init)
(doom-themes-org-config)
(doom-themes-treemacs-config)
(doom-themes-visual-bell-config)

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Treemacs actions
(with-eval-after-load 'treemacs
  (treemacs-follow-mode)
  (treemacs-filewatch-mode)
  (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace))

;; Solaire-mode (foreground buffer color difference)
(require 'solaire-mode)
(solaire-global-mode +1)
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
(add-hook 'after-revert-hook #'turn-on-solaire-mode)
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
(solaire-mode-swap-bg)

;; Rspec-mode side window config
(add-to-list
 'display-buffer-alist
 '("*rspec-compilation" display-buffer-in-side-window (side . bottom) (slot . 2)))

;; Helm
(with-eval-after-load 'helm
  (setq helm-always-two-windows nil)
  (setq helm-display-buffer-default-height 15)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window)))


(add-to-list
 'display-buffer-alist
 '("*MULTI-TERM-DEDICATED*" (display-buffer-in-side-window) (side . bottom) (slot . 1)))

;;;; Environment-specific configuration
;; Mac-specific config
(when (eq system-type 'darwin)
  ;; Before anything starts, get the right envs
  (make-thread (lambda ()
    (when (not (getenv "TERM_PROGRAM"))
      (setenv "PATH" (shell-command-to-string "/bin/cat /etc/paths | /usr/bin/tr '\n' ':'"))
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
    (setq interprogram-paste-function 'copy-from-osx))))

;; Environment variable setup
(if (and (not (eq system-type 'darwin)) (not (getenv "TERM_PROGRAM")))
    (make-thread
     (lambda ()
       (let ((path (shell-command-to-string
                    "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
         (setenv "PATH" path)
         (setq exec-path (split-string path ":"))))))

;;;; Things to run at startup
;; Load keychain env
(keychain-refresh-environment)

;; Git-gutter-fringe
(when (display-graphic-p)
  (autoload 'git-gutter+-mode "git-gutter-fringe+" nil t))

;; Company-mode
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-flow)
  (add-to-list 'company-backends 'company-robe)
  (add-to-list 'company-backends 'company-lsp))

(with-eval-after-load 'flycheck
  (flycheck-popup-tip-mode)
  (require 'flycheck-flow)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#909090"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#D0D000"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "medium sea green"))))
 '(treemacs-git-added-face ((t (:foreground "green"))))
 '(treemacs-git-modified-face ((t (:foreground "dark orange")))))

(custom-set-variables
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
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
     (dot . t)
     (sql . t)
     (ruby . t)
     (restclient . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-src-tab-acts-natively t)
 '(org-support-shift-select t)
 '(rbenv-show-active-ruby-in-modeline nil)
 '(ruby-align-to-stmt-keywords (quote (if begin case)))
 '(ruby-deep-indent-paren nil)
 '(ruby-indent-level 2)
 '(ruby-insert-encoding-magic-comment nil)
 '(tab-width 2)
 '(typescript-indent-level 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))

(provide 'config)
;;; config.el ends here
