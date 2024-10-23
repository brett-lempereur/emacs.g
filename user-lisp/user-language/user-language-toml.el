;;; user-language-toml -- Tom's Own Markup Language support.

;;; Commentary:

;; Support for editing Tom's Own Markup Language files and buffers.

;;; Code:

(require 'use-package)

(require 'user-editing)
(require 'user-hooks)

;; Tom's own markup language support
(use-package toml-mode
  :mode "\\.toml\\'"
  :hook
  ((toml-mode . aggressive-indent-mode)
   (toml-mode . user-markup-minor-modes)))

(provide 'user-language-toml)
;;; user-language-toml.el ends here
