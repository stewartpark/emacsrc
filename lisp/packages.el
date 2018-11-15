;;; packages.el --- Package management
;;;
;;; Commentary:
;;; All the packages to install, and utility functions
;;;
;;; Code:

;; Package setup
(defvar package-archives '(
    ("melpa" . "https://melpa.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("org" . "http://orgmode.org/elpa/")))

(defvar package-list '(
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
    hackernews transpose-frame pinentry
    all-the-icons
    flycheck flycheck-rust flycheck-crystal flycheck-popup-tip
    company company-racer racer company-restclient
    doom-themes restart-emacs cov
    helm-tramp docker-tramp tramp-hdfs
    ace-window multi-term nlinum doom-modeline
    projectile treemacs treemacs-projectile ag editorconfig
    helm helm-projectile helm-ag helm-circe helm-company helm-spotify helm-flycheck swiper-helm
    emojify circe circe-notifications json json-rpc restclient zeal-at-point
    exwm desktop-environment
))


(defun ensure-packages-installed ()
  "Ensure packages listed are installed."
  (interactive)
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;; Ensure packages installed.
(ensure-packages-installed)

(provide 'packages)
;;; packages.el ends here
