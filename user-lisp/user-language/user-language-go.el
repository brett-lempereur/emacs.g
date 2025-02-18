;;; user-language-go -- Go development environment.

;;; Commentary:

;; Provides an integrated development environment for Go, with most
;; functionality provided by the language server protocol.

;;; Code:

(require 'use-package)

(require 'user-hooks)

;; Go language support
(use-package go-mode
  :mode "\\.go\\'"
  :bind
  (:map go-mode-map
        ("C-c C-c C-c" . go-build))
  :hook
  ((go-mode . user-lsp-minor-modes)
   (go-mode . user-programming-minor-modes)
   (before-save . gofmt-before-save))
  :custom
  (gofmt-command "goimports"))

(provide 'user-language-go)
;;; user-language-go.el ends here
