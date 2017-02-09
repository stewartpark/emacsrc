;;; utils.el --- my utility functions and modes
;;; Commentary:
;;; This program defines some useful functions and modes for Stewart Park.
;;; Code:
(require 'url)

(defun http:get (url)
  "Func: (http:get url) sends a HTTP GET request to the URL."
  (let (result)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))

      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))

      (re-search-forward "^$" nil 'move)
      (setq result (buffer-substring-no-properties (point) (point-max)))

      (kill-buffer (current-buffer))

      result)))

(defun font+ ()
  "Cmd: (font+) increases the font size."
  (interactive)
  (let ((new_size (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new_size)))

(defun font- ()
  "Cmd: (font-) decreases the font size."
  (interactive)
  (let ((new_size (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new_size)))

(defun open-init ()
  "Cmd: (open-init) opens the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-lisp ()
  "Cmd: (open-lisp) opens the lisp directory under .emacs.d."
  (interactive)
  (find-file "~/.emacs.d/lisp/"))

(defun get-all-documentations-as-comments ()
  "Get all docstrings from exposed functions."
  (let ((l (make-list 0 nil)))
    (dolist (f utils)
      (add-to-list 'l (concat ";; " (documentation f) "\n")))
    (apply 'concat l)))

(defun toggle-window-split ()
  "Cmd: (toggle-window-split) toggles window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))
    "This supports 2 windows only"))

(defvar utils
  '(http:get font+ font- open-init open-lisp toggle-window-split)
  "A list of every function this file defines.")
(provide 'utils)
;;; utils.el ends here
