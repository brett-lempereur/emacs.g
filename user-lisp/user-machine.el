;;; user-machine -- Machine-specific settings and customisation.

;;; Commentary:

;; Attempts to load a machine-specific configuration file.

;;; Code:

(defun user-machine-init ()
  "Return the symbol of the machine-specific initialisation module."
  (let ((hostname (car (split-string (system-name) "\\."))))
    (intern (downcase (format "user-machine-%s" hostname)))))

;; Attempt to include the machine-specific configuration file, and if that
;; fails then warn the user.
(condition-case error
    (require (user-machine-init))
  (file-missing
   (warn "No machine-specific configuration found: '%s'" (user-machine-init))))

(provide 'user-machine)
;;; user-machine.el ends here
