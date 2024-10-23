;;; user-language-racket -- Racket development environment.

;;; Commentary:

;; Provides an integrated development environment for Racket, with an
;; interactive REPL and test runner agent.

;;; Code:

(require 'use-package)

(require 'user-hooks)

;; Racket language support
(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook
  ((racket-mode . racket-xp-mode)
   (racket-mode . user-lisp-minor-modes)
   (racket-mode . user-programming-minor-modes)
   (racket-mode . user-tex-input-method)
   (racket-repl-mode . racket-xp-mode)
   (racket-repl-mode . user-lisp-minor-modes)
   (racket-repl-mode . user-programming-minor-modes)
   (racket-repl-mode . user-tex-input-method))
  :custom
  (racket-xp-eldoc-level 'minimal))

(use-package scribble-mode
  :mode "\\.scrbl\\'"
  :hook
  ((scribble-mode . user-lisp-minor-modes)
   (scribble-mode . user-programming-minor-modes)))

(provide 'user-language-racket)
;;; user-language-racket.el ends here
