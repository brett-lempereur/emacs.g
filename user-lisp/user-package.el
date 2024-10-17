;;; user-package -- Package management support.

;;; Commentary:

;; Additional tools for the collective to support package discovery and
;; management tasks.

;;; Code:

(require 'use-package)

;; Package database browser
(use-package epkg
  :demand t
  :custom
  (epkg-repository (expand-file-name (locate-user-emacs-file "local/epkg"))))

(provide 'user-package)
;;; user-package.el ends here
