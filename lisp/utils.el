(defun http ()
  (interactive)
  (switch-to-buffer-other-window (generate-new-buffer "*HTTP Request*"))
  (restclient-mode))

(provide 'utils)
