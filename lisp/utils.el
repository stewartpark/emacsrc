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

(defun get-all-documentations-as-comments ()
  "Get all docstrings from exposed functions."
  (let ((l (make-list 0 nil)))
    (dolist (f utils)
      (add-to-list 'l (concat ";; " (documentation f) "\n")))
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

(defvar utils
  '(trim-string http:get file:read show-itunes show-net-latency todo font+ font- open-init open-lisp)
  "A list of every function this file defines.")
(provide 'utils)
;;; utils.el ends here
