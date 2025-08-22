;;; user-language-ocaml.el -- OCaml development environment.

;;; Commentary:

;; This module provides an integrated development environment for OCaml, with
;; environment management and an interactive REPL.

;;; Code:

(require 'use-package)

(require 'user-hooks)

;; Language support
(use-package tuareg
  :mode
  ("\\.ml[ip]?\\'" . tuareg-mode)
  ("\\.eliomi?\\'" . tuareg-mode)
  :hook
  ((tuareg-mode . merlin-mode)
   (tuareg-mode . user-programming-minor-modes)
   (tuareg-mode . utop-minor-mode))
  :custom
  (tuareg-highlight-all-operators t))

;; Integrated development environment support
(use-package merlin
  :commands merlin-mode)

;; Build system language
(use-package dune
  :commands dune-mode
  :mode
  ("dune" . dune-mode)
  ("dune-project" . dune-mode)
  ("dune-workspace" . dune-mode)
  ("dune.inc" . dune-mode)
  :hook
  ((dune-mode . user-programming-minor-modes)))

;; Interactive environment
(use-package utop
  :commands (utop-minor-mode utop)
  :hook
  ((utop-mode . user-repl-minor-modes))
  :custom
  (utop-command "opam exec -- dune utop . -- -emacs")
  (utop-edit-command nil))

(provide 'user-language-ocaml)
;;; user-language-ocaml.el ends here
