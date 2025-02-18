;;; user-language-python -- Python development environment.

;;; Commentary:

;; This module provides an integrated development environment for Python code
;; using the language server protocol for most functionality.

;;; Code:

(require 'python)

(require 'user-development)
(require 'user-helpers)
(require 'user-hooks)

(defun user-python-project-environment ()
  "Attempt to activate an environment for the current project."
  (when-let ((venv-path (find-in-project ".venv")))
    (pyvenv-activate venv-path)))

(defun user-python-project-formatter ()
  "Attempt to activate an automatic formatting mode for the current project."
  (let ((ruff (executable-find "ruff"))
        (black (executable-find "black")))
    (cond
     (ruff (ruff-format-on-save-mode))
     (black (python-black-on-save-mode)))))

;; Language hooks
(add-hook 'python-base-mode-hook #'smartparens-mode)
(add-hook 'python-base-mode-hook #'user-programming-minor-modes)
(add-hook 'python-base-mode-hook #'user-whitespace-significant-modes)
(add-hook 'python-base-mode-hook #'user-lsp-minor-modes)

;; Environment hooks
(add-hook 'python-base-mode-hook #'user-python-project-environment -10)
(add-hook 'python-base-mode-hook #'user-python-project-formatter)
(add-hook 'python-base-mode-hook #'pyvenv-mode)

;; Insert colons automatically with parentheses
(setopt sp-python-insert-colon-in-function-definitions t)

;; Virtual environment discovery
(use-package pyvenv
  :commands (pyvenv-mode pyvenv-activate pyvenv-workon))

;; Automatic formatting
(use-package python-black
  :commands python-black-on-save-mode)
(use-package ruff-format
  :commands ruff-format-on-save-mode)

(provide 'user-language-python)
;;; user-language-python.el ends here
