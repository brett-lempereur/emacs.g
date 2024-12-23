;;; user-hooks -- Common hook declarations.

;;; Commentary:

;; This module provides common hook definitions that are shared between
;; multiple major modes.

;;; Code:

(require 'user-editing)
(require' user-development)

(defun user-programming-minor-modes ()
  "Activates minor modes common to all programming languages."
  ;; Minor modes
  (auto-fill-mode)
  (corfu-mode)
  (display-line-numbers-mode)
  (eldoc-mode)
  (hl-line-mode)
  (hl-todo-mode)
  (hungry-delete-mode)
  (idle-highlight-mode)
  (rainbow-delimiters-mode)
  ;; Configuration
  (setq-local comment-auto-fill-only-comments t))

(defun user-whitespace-significant-modes ()
  "Activates minor modes common to all whitespace significant languages."
  ;; Minor modes
  (highlight-indentation-mode))

(defun user-lsp-minor-modes ()
  "Activates the language server protocol client and features."
  (eglot-ensure)
  (flymake-mode))

(defun user-lisp-minor-modes ()
  "Activates minor modes common to all Lisp languages."
  (aggressive-indent-mode)
  (flycheck-mode)
  (smartparens-strict-mode)
  (show-smartparens-mode))

(defun user-repl-minor-modes ()
  "Activates minor modes common to all REPL buffers."
  ;; Minor modes
  (aggressive-indent-mode)
  (auto-fill-mode)
  (corfu-mode)
  (hl-todo-mode)
  (hungry-delete-mode)
  (idle-highlight-mode)
  (rainbow-delimiters-mode)
  ;; Configuration
  (setq-local comment-auto-fill-only-comments t))

(defun user-markup-minor-modes ()
  "Activates minor modes common to all markup languages."
  (auto-fill-mode)
  (display-line-numbers-mode)
  (hl-todo-mode)
  (flycheck-mode)
  (rainbow-delimiters-mode)
  (smartparens-mode)
  (ws-butler-mode)
  (yas-minor-mode))

(defun user-tex-input-method ()
  "Activates the TeX input method in the current buffer."
  (set-input-method "TeX"))

(provide 'user-hooks)
;;; user-hooks.el ends here
