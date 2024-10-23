;;; early-init -- Early environment initialisation.

;;; Commentary:

;; This initialisation script is evaluated before the main initialisation
;; script to ensure that package management is configured and the load
;; path includes user modules.

;;; Code:

(require 'comp)
(require 'package)

;; Setup the load path
(add-to-list 'load-path (locate-user-emacs-file "user-package/no-littering"))
(add-to-list 'load-path (locate-user-emacs-file "user-package/borg"))
(add-to-list 'load-path (locate-user-emacs-file "user-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "user-lisp/user-language"))
(add-to-list 'load-path (locate-user-emacs-file "user-machine"))

;; Bump the garbage control thresholds to reasonable values.
(setopt gc-cons-threshold (* 1024 1024 192))
(setopt gc-cons-percentage 0.25)

;; Configure native compilation
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (locate-user-emacs-file "local/eln-cache")))
(setopt native-comp-async-query-on-exit t)
(setopt native-comp-async-report-warnings-errors 'silent)

;; Load newer uncompiled files if present
(setopt load-prefer-newer t)

;; Never load site-specific configuration
(setopt inhibit-default-init t)

;; Disable built-in package management
(setopt package-enable-at-startup nil)

;; Keep the configuration directory clean
(setopt no-littering-etc-directory (locate-user-emacs-file "local/config/"))
(setopt no-littering-var-directory (locate-user-emacs-file "local/data/"))
(require 'no-littering)

;; Initialise the collective
(require 'borg)
(borg-initialize)

(provide 'early-init)
;; early-init.el ends here
