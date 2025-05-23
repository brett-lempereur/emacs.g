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
  (:map rust-mode-map
        ("C-c C-c C-c" . rust-compile)
        ("C-c C-c C-l" . rust-run-clippy)
        ("C-c C-c C-t" . rust-test))
  :hook
  ((rust-mode . cargo-minor-mode)
   (rust-mode . user-lsp-minor-modes)
   (rust-mode . user-programming-minor-modes))
  :custom
  (rust-format-on-save t)
  (rust-indent-method-chain t)
  (rust-format-show-buffer nil)
  (rust-rustfmt-switches '("--edition" "2024" "--style-edition" "2024")))

;; Build system support
(use-package cargo
  :commands cargo-minor-mode)

(provide 'user-language-rust)
;;; user-language-rust.el ends here
