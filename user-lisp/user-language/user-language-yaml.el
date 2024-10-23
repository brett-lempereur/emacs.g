;;; user-language-yaml -- Yes Another Markup Language support.

;;; Commentary:

;; Support for editing Yet Another Markup Language files and buffers.

;;; Code:

(require 'use-package)

(require 'user-hooks)

;; Yet another markup language support
(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  :hook
  (yaml-mode-hook . aggressive-indent-mode)
  (yaml-mode-hook . user-markup-minor-modes))

(provide 'user-language-yaml)
;;; user-language-yaml.el ends here
