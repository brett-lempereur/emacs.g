;;; user-language-web -- Web development environment.

;;; Commentary:

;; This module configures a combination of web languages and build systems
;; to provide an integrated development environment for the web.

;;; Code:

(require 'user-hooks)

;; Typescript language hooks
(add-hook 'typescript-ts-mode-hook #'user-programming-minor-modes)
(add-hook 'typescript-ts-mode-hook #'user-lsp-minor-modes)
(add-hook 'typescript-ts-mode-hook #'apheleia-mode)

;; Hypertext markup language hooks
(add-hook 'html-ts-mode-hook #'user-programming-minor-modes)

(provide 'user-language-web)
;;; user-language-web.el ends here
