;;; packages.el --- Package management
;;;
;;; Commentary:
;;; All the packages to install, and utility functions
;;;
;;; Code:

;; Package setup
(setq package-archives '(
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("org" . "http://orgmode.org/elpa/")))

(defvar package-list '(
    better-defaults multiple-cursors
    python-mode lsp-python-ms py-isort py-autopep8 pyenv-mode-auto pythonic
    ruby-mode rspec-mode rbenv inf-ruby robe bundler crystal-mode
    racket-mode elm-mode rust-mode cargo vue-mode rjsx-mode typescript-mode tide
    markdown-mode yaml-mode haskell-mode antlr-mode groovy-mode coffee-mode
    scala-mode sbt-mode lsp-metals flow-minor-mode flycheck-flow
    dockerfile-mode nasm-mode go-mode foreman-mode js2-mode json-mode jest
    ansible scss-mode web-mode rainbow-mode rainbow-delimiters
    smartparens dumb-jump zoom-window dotenv-mode
    fringe-helper git-gutter-fringe+ magit keychain-environment
    org org-present ob-restclient
    hackernews transpose-frame pinentry
    all-the-icons solaire-mode
    flycheck flycheck-rust flycheck-crystal flycheck-popup-tip
    company racer company-restclient company-flow lsp-java company-lsp lsp-ui
    company-inf-ruby company-ansible
    company-jedi company-c-headers company-jedi company-go company-ghc company-shell company-web
    helm-tramp docker-tramp kubernetes-tramp tramp-hdfs restart-emacs
    ace-window multi-term doom-modeline doom-themes eterm-256color
    projectile treemacs treemacs-projectile treemacs-icons-dired treemacs-magit ag editorconfig
    helm helm-projectile helm-ag helm-circe helm-company helm-spotify helm-flycheck swiper helm-smex
    emojify circe circe-notifications json json-rpc restclient zeal-at-point
))

(setq package--init-file-ensured t)

(defun ensure-packages-installed ()
  "Ensure packages listed are installed."
  (interactive)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;; Ensure packages installed.
(ensure-packages-installed)

(provide 'packages)
;;; packages.el ends here
