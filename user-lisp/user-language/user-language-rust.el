;;; user-language-rust -- Rust development environment.

;;; Commentary:

;; Provides an integrated development environment for Rust, with most
;; functionality provided by the language server protocol.

;;; Code:

(require 'use-package)

(require 'user-development)
(require 'user-hooks)

;; Rust language support
(use-package rust-mode
  :mode "\\.rs\\'"
  :bind
  (("C-c C-c c" . rust-compile)
   ("C-c C-c l" . rust-run-clippy)
   ("C-c C-c t" . rust-test))
  :hook
  ((rust-mode . cargo-minor-mode)
   (rust-mode . smartparens-mode)
   (rust-mode . user-lsp-minor-modes)
   (rust-mode . user-programming-minor-modes))
  :custom
  (rust-format-on-save t)
  (rust-indent-method-chain t))

;; Build system support
(use-package cargo
  :commands cargo-minor-mode)

(provide 'user-language-rust)
;;; user-language-rust.el ends here
