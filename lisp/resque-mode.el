;;; resque-mode.el --- a real-time buffer to watch Resque on Emacs

;;; Copyright (C) 2018 Stewart Park

;;; Commentary:

;;; Put this in your Emacs directory and simply:
;;; (require 'resque-mode)
;;; (resque)

;;; requires: s, org-mode

;;; Code:

(defgroup resque nil
  "Options for resque."
  :prefix "resque-"
  :group 'files)

(defcustom resque-base-url nil
  "*Resque URL with a trailing slash."
  :type 'string
  :group 'resque)

(defun resque--send-request ()
  "Send a HTTP GET request to the URL."
  (with-current-buffer
      (url-retrieve-synchronously
       (concat resque-base-url "overview.poll"))
    (prog1
        (buffer-string)
      (kill-buffer))))

(defun resque--remove-queue (queue-name)
  "Remove a queue.  QUEUE-NAME: name of the queue."
  (let ((url-request-method "POST")
        (url-request-data ""))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat resque-base-url "queues/" queue-name "/remove"))
      (prog1
          (buffer-string)
        (kill-buffer)))))

(defun resque--clear-failed ()
  "Clear failed tasks."
  (let ((url-request-method "POST")
        (url-request-data ""))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat resque-base-url "failed/clear"))
      (prog1
          (buffer-string)
        (kill-buffer)))))

(defun resque--get-queues ()
  "List queues."
  (mapcar
   (lambda (x) (cdr x))
   (s-match-strings-all
    "class=\"queue\" href=\"/.+?\">\\(.+?\\)<.+?size'>\\(.+?\\)<"
    (resque--send-request))))

(defun resque--contains (queue-name)
  "QUEUE-NAME: queue name to check."
  (member queue-name (mapcar 'car (resque--get-queues))))

(defun resque--render-string ()
  "Render the string for the buffer."
  (let ((l (make-list 0 nil)))
    (add-to-list 'l "* Resque Monitor\n")
    (add-to-list 'l "|Queue|# of Messages In-flight|\n")
    (add-to-list 'l "|-----+-----------------------|\n")
    (dolist (queues (resque--get-queues))
      (add-to-list 'l
                   (format "| %s | %s |\n" (car queues) (car (cdr queues)))))
    (apply 'concat (reverse l))))

(defun resque--render ()
  (let ((buf (get-buffer "*Resque*")))
    (with-current-buffer buf
      (read-only-mode 0)
      (erase-buffer)
      (insert (resque--render-string))
      (org-table-align)
      (read-only-mode 1))))

(define-derived-mode resque-mode org-mode "Resque"
  "Major mode for watching resque web"
  (progn
    (local-set-key (kbd "r") (lambda ()
                               (interactive)
                               (resque--render)
                               (message "Resque refreshed.")))
    (local-set-key (kbd "c") (lambda ()
                               (interactive)
                               (if (resque--contains (current-word))
                                   (when (y-or-n-p (format "Clear %s? " (current-word)))
                                     (if (string= "failed" (current-word))
                                         (resque--clear-failed)
                                       (resque--remove-queue (current-word)))
                                     (message (format "%s cleared." (current-word)))
                                     (resque--render))
                                 (message "Select queue to clear."))))

    (resque--render)
    (message "Resque Monitor")))

(defun resque ()
  "Enable resque-mode."
  (interactive)
  (when (not (eq (buffer-name) "*Resque"))
      (switch-to-buffer "*Resque*"))
  (resque-mode))

(provide 'resque-mode)
;;; resque-mode.el ends here
