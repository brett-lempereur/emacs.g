;;; user-helpers -- Helper functions and macros.

;;; Commentary:

;; This module provides helper functions and macros used in other parts of the
;; configuration.

;;; Code:

(defun package-subdirectory (package &rest path)
  "Return the path to subdirectory at PATH in PACKAGE."
  (let ((package-path (file-name-directory (locate-library package))))
    (apply #'file-name-concat package-path path)))

(defun add-package-subdirectory-to-load-path (package &rest path)
  "Add the path to subdirectory PATH in PACKAGE to the load path."
  (add-to-list 'load-path (apply #'package-subdirectory package path)))

(defun find-executable (command)
  "Return the path to executable COMMAND or nil if it does not exist."
  (locate-file command exec-path exec-suffixes 1))

(defun find-in-project (path)
  "Return the absolute path to PATH in the current project, if it exists.

Returns nil if no project is active."
  (if (not (project-current)) nil
    (let* ((project (project-root (project-current)))
           (local-path (concat (file-name-as-directory project) path)))
      (if (file-directory-p local-path) local-path nil))))

(provide 'user-helpers)
;;; user-helpers.el ends here
