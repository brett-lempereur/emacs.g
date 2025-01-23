;;; user-machine-mahoro -- Configuration for my laptop.

;;; Commentary:

;; Configures a development environment for hobby projects.

;;; Code:

(require 'user-settings)

;; Appearance customisations
(setopt user-setting-font '(("Monaspace Argon" . 14) ("Menlo" . 13)))
(setopt user-setting-line-spacing 2)

;; Editing customisations
(setopt user-setting-claude-api-key "ANTHROPIC_API_KEY")

;; Operating system customisations
(setopt user-setting-shell-variables '("PATH" "ANTHROPIC_API_KEY"))

;; Project management customisations
(setopt user-setting-project-paths '("~/Workspace" "~/Projects"))
(setopt user-setting-notebook-path "~/Documents/Notes/")

(provide 'user-machine-mahoro)
;;; user-machine-mahoro.el ends here
