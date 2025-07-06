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
(defun homebrew-gcc-paths ()
  "Return GCC library paths from Homebrew installations.
Detects paths for gcc and libgccjit packages to be used in LIBRARY_PATH."
  (let* ((paths '())
         (brew-bin (or (executable-find "brew")
                       (let ((arm-path "/opt/homebrew/bin/brew")
                             (intel-path "/usr/local/bin/brew"))
                         (cond
                          ((file-exists-p arm-path) arm-path)
                          ((file-exists-p intel-path) intel-path))))))

    (when brew-bin
      ;; Get gcc paths.
      (let* ((gcc-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix gcc"))))
             (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
        (push gcc-lib-current paths)

        ;; Find apple-darwin directory.
        (let* ((default-directory gcc-lib-current)
               (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
          (when arch-dirs
            (push (expand-file-name
                   (car (sort arch-dirs #'string>)))
                  paths))))

      ;; Get libgccjit paths
      (let* ((jit-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix libgccjit"))))
             (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
        (push jit-lib-current paths)))

    (nreverse paths)))

(defun setup-macos-native-comp-library-paths ()
  "Set up LIBRARY_PATH for native compilation on macOS.
Includes Homebrew GCC paths and CommandLineTools SDK libraries."
  (let* ((existing-paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t))
         (gcc-paths (homebrew-gcc-paths))
         (clt-paths '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
         (unique-paths (delete-dups
                        (append existing-paths gcc-paths clt-paths))))

    (setenv "LIBRARY_PATH" (mapconcat #'identity unique-paths ":"))))
(when (eq system-type 'darwin)
  (setup-macos-native-comp-library-paths))
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

;; Initialise the collective
(require 'borg)
(borg-initialize)

;; Keep the configuration directory clean
(setopt no-littering-etc-directory (locate-user-emacs-file "local/config/"))
(setopt no-littering-var-directory (locate-user-emacs-file "local/data/"))
(require 'no-littering)

(provide 'early-init)
;; early-init.el ends here
