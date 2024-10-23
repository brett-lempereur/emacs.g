;;; user-language-common-lisp -- Common Lisp development environment.

;;; Commentary:

;; Provides an integrated development environment for Common Lisp, with an
;; interactive REPL and documentation support.

;;; Code:

(require 'lisp-mode)

(require 'use-package)

(require 'user-hooks)
(require 'user-settings)

;; Additional minor modes
(add-hook 'lisp-mode-hook #'user-programming-minor-modes)
(add-hook 'lisp-mode-hook #'user-lisp-minor-modes)
(add-hook 'lisp-mode-hook #'slime)

;; Integrated development environment
(use-package slime
  :commands (slime slime-mode)
  :hook
  (slime-repl-mode . user-programming-minor-modes)
  (slime-repl-mode . user-lisp-minor-modes)
  :custom
  (inferior-lisp-program user-setting-lisp-program)
  :config
  (add-to-list 'slime-contribs 'slime-asdf)
  (add-to-list 'slime-contribs 'slime-autodoc)
  (add-to-list 'slime-contribs 'slime-c-p-c)
  (add-to-list 'slime-contribs 'slime-editing-commands)
  (add-to-list 'slime-contribs 'slime-mdot-fu)
  (add-to-list 'slime-contribs 'slime-presentations)
  (add-to-list 'slime-contribs 'slime-references)
  (add-to-list 'slime-contribs 'slime-repl)
  (add-to-list 'slime-contribs 'slime-xref-browser))

(provide 'user-language-common-lisp)
;;; user-language-common-lisp.el ends here
