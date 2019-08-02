;;; init.el --- Emacs boot up code
;;;
;;; Commentary:
;;; Stewart Park's Emacs configuration.
;;;
;;; Code:

;;; TODO: use use-package, smart-jump

(let ((file-name-handler-alist nil))
  ;; Make startup faster by reducing the frequency of garbage collection.
  (setq gc-cons-threshold (* 1024 1024 1024) ; 1 GiB
        gc-cons-percentage 1.0)

  ;; Load packages
  (load "~/.emacs.d/lisp/packages.el")

  ;; Custom utillities and modes
  (load "~/.emacs.d/lisp/utils.el")
  (load "~/.emacs.d/lisp/resque-mode.el")

  ;; Custom configuartion files
  (load "~/.emacs.d/lisp/config.el")
  (load "~/.emacs.d/lisp/hooks.el")
  (load "~/.emacs.d/lisp/keys.el")
  (load "~/.emacs.d/lisp/file-types.el")

  ;; Host-specific configurations
  (if (file-exists-p "~/.config/emacs.el")
      (load "~/.config/emacs.el"))

  ;; Clean up and add projects to projectile in a thread
  (let ((project-paths (-non-nil (-flatten (projectile-discover-projects-in-search-path)))))
    (projectile-cleanup-known-projects)
    (make-thread
     (lambda ()
       (dolist (project-path project-paths)
         (thread-yield)
         (projectile-add-known-project project-path)))))

  ;; Things I need to turn on in the beginning
  (helm-projectile-on)
  (pinentry-start)
  (server-start)

  ;; Make gc pauses faster by decreasing the threshold.
  (setq gc-cons-threshold (* 1 1024 1024) ; 1MiB
        gc-cons-percentage 0.1))

(provide 'init)

;;; init.el ends here
