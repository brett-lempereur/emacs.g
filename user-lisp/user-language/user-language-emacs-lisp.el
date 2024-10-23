;;; user-language-emacs-lisp -- Emacs lisp development environment.

;;; Commentary:

;; Configures and provides minor modes that make writing Emacs Lisp an
;; even more pleasant experience.

;;; Code:

(require 'elisp-mode)

(require 'use-package)

(require 'user-hooks)

;; Hooks
(add-hook 'emacs-lisp-mode-hook #'eros-mode)
(add-hook 'emacs-lisp-mode-hook #'user-programming-minor-modes)
(add-hook 'emacs-lisp-mode-hook #'user-lisp-minor-modes)

;; Better documentation browsing and rendering
(use-package helpful
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d" . helpful-at-point)
        ("C-h f" . helpful-callable)
        ("C-h v" . helpful-variable)
        ("C-h k" . helpful-key))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Interactive code valuation
(use-package eros)

(provide 'user-language-emacs-lisp)
;;; user-language-emacs-lisp.el ends here
