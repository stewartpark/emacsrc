;;; utils.el --- my utility functions and modes
;;; Commentary:
;;; This program defines some useful functions and modes for Stewart Park.
;;; Code:
(require 'url)
(require 'dash)

(defvar saved-treemacs-state nil)

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

(defun treemacs-show ()
  "Ensure treemacs is showed."
  (ignore-errors
    (pcase (treemacs-current-visibility)
      ('exists (treemacs-select-window))
      ('none (treemacs--init)))))

(defun treemacs-hide ()
  "Ensure treemacs is hidden."
  (ignore-errors
    (if (eq (treemacs-current-visibility) 'visible)
        (delete-window (treemacs-get-local-window)))))

(defun save-treemacs-state ()
  "Save treemacs's state."
  (ignore-errors
    (setq saved-treemacs-state
          (pcase (treemacs-current-visibility)
            ('visible t)
            ('exists nil)
            ('none nil)))))

(defun restore-treemacs-state ()
  "Restore treemacs's state."
  (ignore-errors
    (if saved-treemacs-state
      (treemacs-show)
      (treemacs-hide))))

(defun todo ()
  "Cmd: (todo) open a todo org."
  (interactive)
  (find-file "~/todo.org"))

(defun get-net-latency ()
  "Cmd: (show-net-latency) show network latency."
  (interactive)
  (let
    ((latency (nth 3 (split-string (nth 1 (split-string (shell-command-to-string "ping -c 1 8.8.8.8") "\n")) "="))))
    (if (eq latency nil)
      "Offline"
      latency)))

(defun get-active-net-interface-name (type)
  "Return the name of an active network interface whose type is the given TYPE."
  (car
   (car
    (-filter
     (lambda (x) (string= (nth 2 x) type))
     (delete
      '("")
      (mapcar
       (lambda (x) (split-string x ":"))
       (split-string
        (trim-string
         (shell-command-to-string "nmcli -t c show --active | tr '\n' ';'")) ";")))))))

(defun get-wifi-name ()
  "Return the currently connected access point's advertised name."
  (get-active-net-interface-name "802-11-wireless"))

(defun get-vpn-name ()
  "Return the currently connected VPN's name."
  (get-active-net-interface-name "vpn"))

(defun get-ethernet-name ()
  "Return the currently connected ethernet's name."
  (get-active-net-interface-name "802-3-ethernet"))

(defun get-net-ipv4-addr (interface)
  "Return the IPv4 address of the given INTERFACE."
  (car (cdr (split-string (trim-string (shell-command-to-string (concat "nmcli -t c show '" interface "' | grep IP4.ADDRESS"))) ":"))))

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
         '(85 . 85) '(100 . 100)))))

(defun compile-lisp ()
  "Cmd: (compile-lisp) Compile all the custom Lisp files."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/lisp/" 0))

(defvar utils
  '(enter-fullscreen quit-fullscreen trim-string http:get file:read get-net-latency get-ethernet-name get-wifi-name get-net-ipv4-addr todo font+ font- open-init open-lisp open-local kill-whitespace-or-word linum-update-window-scale-fix sticky-buffer-mode load-random-theme toggle-transparency compile-lisp)
  "A list of every function this file defines.")
(provide 'utils)
;;; utils.el ends here
