(defun open-init ()
  "Cmd: (open-init) open the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-lisp ()
  "Cmd: (open-lisp) open the Lisp directory under .emacs.d."
  (interactive)
  (find-file "~/.emacs.d/lisp/"))

(defun http ()
  "Cmd: (http) call a HTTP endpoint."
  (interactive)
  (switch-to-buffer-other-window (generate-new-buffer "*HTTP Request*"))
  (restclient-mode))

(defun kill-whitespace-or-word-backward ()
  "Smart delete like a modern editor."
  (interactive)
  (if (looking-back "[ \t\n]" 1)
      (let ((p (point)))
        (re-search-backward "[^ \t\n]" nil :no-error)
        (forward-char)
        (kill-region p (point)))
    (backward-kill-word 1)))

(defun todo ()
  "Cmd: (todo) open a todo file."
  (interactive)
  (find-file "~/todo.org"))

(provide 'utils)
