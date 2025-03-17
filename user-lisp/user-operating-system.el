;;; user-operating-system -- Operating system integration.

;;; Commentary:

;; Provides operating system integration, filesystem navigation and management,
;; and terminal support.

;;; Code:

(require 'use-package)

(require 'user-helpers)
(require 'user-settings)

;; Increase the terminal buffer size
(setopt term-buffer-maximum-size 16384)

;; Open a terminal with a simple keybinding
(global-set-key (kbd "C-c t") #'eat-project)
(global-set-key (kbd "C-c C-t") #'eat)

;; Optionally load environment variables from the shell
(use-package exec-path-from-shell
  :when user-setting-shell-variables
  :custom
  (exec-path-from-shell-variables user-setting-shell-variables)
  :config
  (exec-path-from-shell-initialize))

;; Terminal emulator
(use-package eat
  :bind
  (("C-c t" . eat-project)
   ("C-c C-t" . eat)
   :map eat-mode-map
   ("M-p" . eat-send-password)
   ("M-k" . eat-reset))
  :custom
  (eat-enable-mouse t)
  (eat-kill-buffer-on-exit t)
  :config
  (setopt
   eat-semi-char-non-bound-keys
   (append
    (list (vector meta-prefix-char ?o)
          (vector meta-prefix-char ?p)
          (vector meta-prefix-char ?k))
    eat-semi-char-non-bound-keys)))

;; Multi-window filesystem management
(use-package ranger
  :demand t
  :bind
  (("C-c f r" . ranger)
   ("C-c f d" . deer))
  :custom
  (ranger-cleanup-eagerly t)
  (ranger-hide-cursor t)
  :config
  (ranger-override-dired-mode t))

;; Docker file syntax support
(use-package dockerfile-mode
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode)
  ("\\Containerfile\\'" . dockerfile-mode)
  :custom
  (dockerfile-build-pull t)
  (dockerfile-mode-command user-setting-docker-command)
  (dockerfile-use-buildkit t))

;; Docker management support
(use-package docker
  :when (find-executable user-setting-docker-command)
  :commands (docker docker-compose)
  :bind
  ("C-c d d" . docker)
  ("C-c d l" . docker-containers)
  ("C-c d c" . docker-compose)
  ("C-c d i" . docker-images)
  :custom
  (docker-command user-setting-docker-command)
  (docker-compose-command user-setting-docker-compose-command))

(provide 'user-operating-system)
;;; user-operating-system.el ends here
