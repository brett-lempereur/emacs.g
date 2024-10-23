;;; user-machine-saito -- Configuration for my development laptop.

;;; Commentary:

;; Configures the environment for my development laptop, ensuring the path is
;; loaded from the shell.

;;; Code:

(require 'user-settings)

;; Appearance customisations
(setopt user-setting-font '(("Fira Code" . 13) ("Menlo" . 13)))
(setopt user-setting-line-spacing 2)

;; Operating system customisations
(setopt user-setting-shell-variables '("PATH"))

;; Project management customisations
(setopt user-setting-project-paths '(("~/Workspace" . 1) ("~/Projects" . 1)))
(setopt user-setting-project-tags-command "ctags -f \"%s\" %s \"%s\"")

(provide 'user-machine-saito)
;;; user-machine-saito.el ends here
