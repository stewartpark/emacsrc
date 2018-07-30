;;; utils.el --- my utility functions and modes
;;; Commentary:
;;; This program defines some useful functions and modes for Stewart Park.
;;; Code:
(require 'url)

(defun trim-string (s)
  "Func: (trim-string S) trim trailing whitespaces."
  (replace-regexp-in-string "[ \t\n\r]*$" "" s))

(defun http:get (url)
  "Func: (http:get url) send a HTTP GET request to the URL."
  (let (result)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))

      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))

      (re-search-forward "^$" nil 'move)
      (setq result (buffer-substring-no-properties (point) (point-max)))

      (kill-buffer (current-buffer))

      result)))

(defun file:read (file)
  "Func: (file:read FILE) read file content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun font+ ()
  "Cmd: (font+) increase the font size."
  (interactive)
  (let ((new_size (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new_size)))

(defun font- ()
  "Cmd: (font-) decrease the font size."
  (interactive)
  (let ((new_size (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new_size)))

(defun open-init ()
  "Cmd: (open-init) open the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-lisp ()
  "Cmd: (open-lisp) open the Lisp directory under .emacs.d."
  (interactive)
  (find-file "~/.emacs.d/lisp/"))

(defun open-local ()
  "Cmd: (open-local) open the host-specific Lisp code."
  (interactive)
  (find-file "~/.config/emacs.el"))

(defun get-all-documentations-as-comments ()
  "Get all docstrings from exposed functions."
  (let ((l (make-list 0 nil)))
    (dolist (f utils)
      (when (or
             (string-prefix-p "Cmd:" (documentation f))
             (string-prefix-p "Func:" (documentation f)))
          (add-to-list 'l (concat ";; " (documentation f) "\n"))))
    (apply 'concat l)))

(defun save-neotree-state ()
  "Save neotree's state."
  (setq saved-neotree-state (neo-global--window-exists-p)))

(defun restore-neotree-state ()
  "Restore neotree's state."
  (if saved-neotree-state
      (neotree-show)
      (neotree-hide)))

(defun todo ()
  "Cmd: (todo) open a todo org."
  (interactive)
  (find-file "~/todo.org"))

(defun show-itunes ()
  "Cmd: (show-itunes) show what song iTunes is playing."
  (interactive)
  (let (
        (artist (trim-string (shell-command-to-string "osascript -e 'tell application \"iTunes\" to artist of current track as string'")))
        (title (trim-string (shell-command-to-string "osascript -e 'tell application \"iTunes\" to name of current track as string'"))))
    (princ (format "♫ %s - %s" artist title))))

(defun show-net-latency ()
  "Cmd: (show-net-latency) show network latency."
  (interactive)
  (let
    ((latency (nth 3 (split-string (nth 1 (split-string (shell-command-to-string "ping -t 1 -c 1 8.8.8.8") "\n")) "="))))
    (princ (if (eq latency nil)
        "Offline"
        latency))))

(defun enter-fullscreen ()
  "Cmd: (enter-fullscreen) enter full screen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun quit-fullscreen ()
  "Cmd: (quit-fullscreen) quit full screen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen nil))

(defun fullscreen-p ()
  (frame-parameter nil 'fullscreen))

(defun save-fullscreen-state ()
  (setq saved-fullscreen-state (fullscreen-p)))

(defun restore-fullscreen-state ()
  (set-frame-parameter nil 'fullscreen saved-fullscreen-state))

(defun linum-update-window-scale-fix (win)
  "Line number font size fix.  WIN: linum-update-window argument."
  (set-window-margins
   win
   (ceiling (* (if (boundp 'text-scale-mode-step)
                   (expt text-scale-mode-step
                         text-scale-mode-amount) 1)
               (if (car (window-margins))
                   (car (window-margins)) 1)
               ))))

(defun kill-whitespace-or-word ()
  "Smart delete like a modern editor."
  (interactive)
  (if (looking-back "[ \t\n]" 1)
      (let ((p (point)))
        (re-search-backward "[^ \t\n]" nil :no-error)
        (forward-char)
        (kill-region p (point)))
    (backward-kill-word 1)))

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

(defun load-random-theme (themes)
  "Load a random theme from a list of THEMES given."
  (load-theme
   (nth (random (length themes)) themes) t))

(defun neotree-project-root-dir ()
  "Make neotree display the right root for the project."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name))
        (win (selected-window)))
    (if (neo-global--window-exists-p)
      (progn
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))
        (select-window win))
    )))

(defun toggle-transparency ()
  "Cmd: (toggle-transparency) Make the window transparent."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

(defvar utils
  '(enter-fullscreen quit-fullscreen trim-string http:get file:read show-itunes show-net-latency todo font+ font- open-init open-lisp open-local kill-whitespace-or-word linum-update-window-scale-fix sticky-buffer-mode load-random-theme neotree-project-root-dir toggle-transparency)
  "A list of every function this file defines.")
(provide 'utils)
;;; utils.el ends here
