;;; user-networking -- Network clients and debugging.

;;; Commentary:

;; This module provides tools for interacting with and debugging network
;; services.

;;; Code:

(require 'user-hooks)

;; Repesentational state transfor client and debugging mode
(use-package restclient
  :mode "\\.http\\'"
  :hook
  ((restclient-mode . user-markup-minor-modes))
  :custom
  (restclient-same-buffer-response t)
  (restclient-same-buffer-response-name "*REST Response*")
  (restclient-inhibit-cookies t))

;; Hypertext transform protocol reference
(use-package know-your-http-well
  :commands (http-method http-header http-status-code http-relation))

(provide 'user-networking)
;;; user-networking.el ends here
