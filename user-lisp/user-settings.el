;;; user-settings -- Customisation options for the environment.

;;; Commentary:

;; Both the environment configuration modules and the per-machine
;; setting modules need these options to be defined.  So, they are
;; provided in a third module.

;;; Code:

(require 'custom)

;; Define a customisation group for machine-specific settings.
(defgroup user-settings nil
  "Machine specific customisations."
  :version "28.0"
  :group 'local)

;; Appearance customisations
(defcustom user-setting-font '(("Fira Code" . 12) ("Monospace" . 12))
  "The font face to use for graphical frames."
  :group 'user-settings
  :type '(repeat (cons string number)))
(defcustom user-setting-line-spacing nil
  "The amount of spacing between each line."
  :group 'user-settings
  :type '(number))
(defcustom user-setting-theme-package 'zenburn-theme
  "The package that contains the custom theme."
  :group 'user-settings
  :type '(choice (const :tag "None" nil) symbol))
(defcustom user-setting-theme 'zenburn
  "The name of the custom theme."
  :group 'user-settings
  :type '(symbol))
(defcustom user-setting-menu-bar-mode nil
  "Whether to display the menu bar."
  :group 'user-settings
  :type '(choice boolean number))
(defcustom user-setting-cookie-file (locate-user-emacs-file "user-data/cookie")
  "The file that contains fortunes and aphorisms."
  :group 'user-settings
  :type '(string))

;; Editing customisations
(defcustom user-setting-claude-api-key nil
  "The API key for Claude."
  :group 'user-settings
  :type '(string))
(defcustom user-setting-claude-model 'claude-3-7-sonnet-20250219
  "The Claude model to use."
  :group 'user-settings
  :type '(symbol))

;; Operating system customisations
(defcustom user-setting-shell-variables '()
  "The list of environment variables to load from the shell."
  :group 'user-settings
  :type '(repeat string))
(defcustom user-setting-docker-compose-command "docker-compose"
  "The name or path of the docker-compose command."
  :group 'user-settings
  :type '(string))
(defcustom user-setting-docker-command "docker"
  "The name or path of the docker command."
  :group 'user-settings
  :type '(string))

;; Project management customisations
(defcustom user-setting-project-indexing-method 'alien
  "The method to use when indexing project files."
  :group 'user-settings
  :type '(symbol))
(defcustom user-setting-project-paths nil
  "The list of paths that contain projects."
  :group 'user-settings
  :type '(repeat (choice string (cons string number))))
(defcustom user-setting-project-tags-command "ctags -Re -f \"%s\" %s \"%s\""
  "The command used to generate tags files in projects."
  :group 'user-settings
  :type '(string))

;; Organisation customisations
(defcustom user-setting-notebook-path "~/Notes/"
  "The path that contains notebooks."
  :group 'user-settings
  :type '(string))
(defcustom user-setting-notebook-projects nil
  "The list of projects documented in user notebooks."
  :group 'user-settings
  :type '(repeat string))

;; Python customisations
(defcustom user-setting-conda-environment-path nil
  "The path to the conda environment store."
  :group 'user-settings
  :type '(string))

;; Common lisp customisations
(defcustom user-setting-common-lisp-program nil
  "The name or path of the Common Lisp binary."
  :group 'user-settings
  :type '(string))

(provide 'user-settings)
;;; user-settings.el ends here
