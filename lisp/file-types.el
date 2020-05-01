;;; file-types.el --- Auto modes for different type types
;;;
;;; Commentary:
;;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))
(add-to-list 'auto-mode-alist '("Procfile\\'" . foreman))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . (lambda () (web-mode) (typescript-mode))))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . (lambda () (js2-mode) (rjsx-minor-mode))))
(add-to-list 'auto-mode-alist '("\\.req" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.scala" . scala-mode))

(provide 'file-types)
;;; file-types.el ends here
