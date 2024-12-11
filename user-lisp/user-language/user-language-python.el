;;; user-language-python -- Python development environment.

;;; Commentary:

;; This module provides an integrated development environment for Python code
;; using the language server protocol for most functionality.

;;; Code:

(require 'python)

(require 'user-development)
(require 'user-hooks)

(defun user-python-project-environment ()
  "Attempt to activate an environment for the current project."
  (condition-case error
      (let* ((project (project-root (project-current)))
             (local-path (concat (file-name-as-directory project) ".venv")))
        (if (file-directory-p local-path)
            (pyvenv-activate local-path)
          (message "Could not active project environment, does not exist")))
    (cl-no-applicable-method
     (message "Could not active project environment, not in project"))))

(defun user-python-project-formatter ()
  "Attempt to activate an automatic formatting mode for the current project."
  (let ((ruff (executable-find "ruff"))
        (black (executable-find "black")))
    (cond
     (ruff (ruff-format-on-save-mode))
     (black (python-black-on-save-mode)))))

;; Language hooks
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'user-programming-minor-modes)
(add-hook 'python-mode-hook #'user-space-indentation-modes)
(add-hook 'python-mode-hook #'user-lsp-minor-modes)

;; Environment hooks
(add-hook 'python-mode-hook #'user-python-project-environment -10)
(add-hook 'python-mode-hook #'user-python-project-formatter)
(add-hook 'python-mode-hook #'pyvenv-mode)

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
