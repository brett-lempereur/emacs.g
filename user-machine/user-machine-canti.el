;;; user-machine-canti -- Configuration for my laptop.

;;; Commentary:

;; Configures a development environment for hobby projects.

;;; Code:

(require 'user-settings)

;; Appearance customisations
(setopt user-setting-font '(("Fira Code" . 14) ("Monospace" . 13)))
(setopt user-setting-line-spacing 2)
(setopt user-setting-menu-bar-mode -1)

;; Editing customisations
(setopt user-setting-claude-api-key "ANTHROPIC_API_KEY")

;; Project management customisations
(setopt user-setting-project-paths '("~/Workspace" "~/Projects"))
(setopt user-setting-notebook-path "~/Documents/Notes/")

(provide 'user-machine-canti)
;;; user-machine-canti.el ends here
