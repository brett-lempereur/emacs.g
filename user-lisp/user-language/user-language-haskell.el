;;; user-language-haskell -- Haskell development environment.

;;; Commentary:

;; This module provides an integrated development environment for Haskell, with
;; an interactive REPL.

;;; Code:

(require 'use-package)

(require 'user-hooks)

;; Language support
(use-package haskell-mode
  :mode
  ("\\.hs\\'" . haskell-mode)
  ("\\.l[gh]s\\'" . haskell-literate-mode)
  ("\\.hsig\\'" . haskell-mode)
  ("\\.[gh]s\\'" . haskell-mode)
  :hook
  (haskell-mode . smartparens-mode)
  (haskell-mode . user-programming-minor-modes)
  (haskell-mode . user-lsp-minor-modes)
  (haskell-literate-mode . smartparens-mode)
  (haskell-literate-mode . user-programming-minor-modes)
  (haskell-literate-mode . user-lsp-minor-modes)
  :custom
  (haskell-electric-indentation-flag t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-type 'cabal-repl)
  (haskell-stylish-on-save t))

(provide 'user-language-haskell)
;;; user-language-haskell.el ends here
