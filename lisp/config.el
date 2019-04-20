;;; config.el --- Emacs configuration
;;;
;;; Commentary:
;;; Anything related to variables, themes, and faces.
;;;
;;; Code:

;; Font setup
(let ((font-face "NanumGothicCoding-13"))
  (set-face-attribute 'default nil :font font-face)
  (set-face-attribute 'variable-pitch nil :font font-face)
  (set-frame-font font-face nil t)
  (setq default-frame-alist '((font . "NanumGothicCoding-13")))
  (set-fontset-font "fontset-default" '(#xac00 . #xd7a3) "NanumGothicCoding-13"))

;; Line number font size fix
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;; Load theme
(load-theme 'srcery t)

;; Global mode settings
(make-thread (lambda ()
  (global-magit-file-mode 1)))

(global-visual-line-mode 1)
(doom-modeline-init)
(ace-window-display-mode t)
(doom-themes-org-config)
(doom-themes-treemacs-config)
(doom-themes-visual-bell-config)
(xterm-mouse-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)

(when (display-graphic-p)
  (set-frame-parameter nil 'undecorated t)
  (scroll-bar-mode 0))

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
 make-backup-files nil
 create-lockfiles nil
 vc-follow-symlinks t
 custom-file (concat user-emacs-directory "/lisp/config.el")

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

 epa-pinentry-mode 'loopback
 require-final-newline t
)

;; Treemacs actions
(with-eval-after-load 'treemacs
  (treemacs-follow-mode)
  (treemacs-filewatch-mode)
  (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace))

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
  (flycheck-add-mode 'javascript-eslint 'floiw-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

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
 '(web-mode-markup-indent-offset 2)
)

(provide 'config)
;;; config.el ends here
