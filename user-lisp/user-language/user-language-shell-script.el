;;; user-language-shell-script -- Shell script development environment.

;;; Commentary:

;; Configures minor modes for shell script development.

;;; Code:

(require 'sh-script)
(require 'use-package)

(require 'user-hooks)

;; Hooks
(add-hook 'sh-mode-hook #'user-programming-minor-modes)

(provide 'user-language-shell-script)
;;; user-language-shell-script.el ends here
