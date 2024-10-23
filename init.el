;;; init -- Editor initialisation.

;;; Commentary:

;; All initialisation is contained within the `user-lisp` path in a set
;; of modules prefixed with `user`.  This script ensures that they are
;; imported in something resembling a correct order.

;;; Code:

;; Backwards compatibility for systems without `early-init.el` support
(require 'early-init (locate-user-emacs-file "early-init.el"))

;; Load machine-specific customisations
(require 'user-machine)

;; General customisation
(require 'user-appearance)
(require 'user-completion)
(require 'user-development)
(require 'user-editing)
(require 'user-networking)
(require 'user-notes)
(require 'user-operating-system)
(require 'user-package)

;; Programming and markup languages
(require 'user-language-common-lisp)
(require 'user-language-emacs-lisp)
(require 'user-language-haskell)
(require 'user-language-json)
(require 'user-language-markdown)
(require 'user-language-ocaml)
(require 'user-language-python)
(require 'user-language-racket)
(require 'user-language-rust)
(require 'user-language-shell-script)
(require 'user-language-toml)
(require 'user-language-yaml)

(provide 'init)
;;; init.el ends here
