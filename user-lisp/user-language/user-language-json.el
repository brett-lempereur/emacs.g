;;; user-language-json -- JavaScript Object Notation support.

;;; Commentary:

;; Support for editing JavaScript Object Notation files and buffer.

;;; Code:

(require 'use-package)

(require 'user-hooks)

;; Language support
(use-package json-mode
  :mode
  ("\\.json" . json-mode)
  :hook
  (json-mode . user-markup-minor-modes))

(provide 'user-language-json)
;;; user-language-json.el ends here
