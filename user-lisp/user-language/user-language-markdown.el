;;; user-language-markdown -- Markdown syntax support.

;;; Commentary:

;; This module adds support for editing markdown documentation.

;;; Code:

(require 'use-package)

(require 'user-hooks)

;; Markdown language support
(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  ("LICENSE\\.md\\'" . gfm-mode)
  :hook
  ((markdown-mode . user-markup-minor-modes)
   (markdown-mode . visual-fill-column-mode)))

(provide 'user-language-markdown)
;;; user-language-markdown.el ends here
