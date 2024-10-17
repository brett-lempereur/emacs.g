;;; init -- Editor initialisation

;;; Commentary:

;; All initialisation is contained within the `user-lisp` path in a set
;; of modules prefixed with `user`.  This script ensures that they are
;; imported in something resembling a correct order.

;;; Code:

;; Backwards compatibility for systems without `early-init.el` support
(require 'early-init (locate-user-emacs-file "early-init.el"))

;; General customisation
(require 'user-package)

(provide 'init)
;;; init.el ends here

