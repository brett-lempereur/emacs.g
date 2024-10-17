;;; early-init -- Early environment initialisation.

;;; Commentary:

;; This initialisation script is evaluated before the main initialisation
;; script to ensure that package management is configured and the load
;; path includes user modules.

;;; Code:

(require 'comp)
(require 'package)

;; Setup the load path
(add-to-list 'load-path (locate-user-emacs-file "user-package/borg"))
(add-to-list 'load-path (locate-user-emacs-file "user-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "user-lisp/user-language"))
(add-to-list 'load-path (locate-user-emacs-file "user-machine"))

;; Configure native compilation
(customize-set-variable 'native-comp-async-query-on-exit t)
(customize-set-variable 'native-comp-async-report-warnings-errors 'silent)

;; Never load site-specific configuration
(setq inhibit-default-init t)

;; Disable built-in package management
(setq package-enable-at-startup nil)

;; Initialise the collective
(require 'borg)
(borg-initialize)

(provide 'early-init)
;; early-init.el ends here

