;;; user-operating-system -- Operating system integration.

;;; Commentary:

;; Provides operating system integration, filesystem navigation and management,
;; and terminal support.

;;; Code:

(require 'use-package)

(require 'user-settings)

(defun user-open-shell ()
  "Open a new shell terminal."
  (interactive)
  (let ((shell (getenv "SHELL")))
    (if shell (ansi-term shell) (ansi-term))))

(defun user-switch-to-or-open-shell ()
  "Switch to an existing shell terminal or open a new terminal."
  (interactive)
  (let* ((buffer (car
                  (sort
                   (seq-filter
                    (lambda (buf)
                      (string-prefix-p "*ansi-term" (buffer-name buf)))
                    (buffer-list)))))
         (window (get-buffer-window buffer)))
    (cond
     ((and buffer window) (select-window window))
     (buffer (switch-to-buffer buffer))
     (t (user-open-shell)))))

(defun user-handle-terminal-exit (&optional process-name msg)
  "Close terminal buffers and displays PROCESS-NAME and MSG."
  (message "Terminal: %s - %s" process-name msg)
  (kill-buffer (current-buffer)))

;; Increase the terminal buffer size
(setopt term-buffer-maximum-size 16384)

;; Open a terminal with a simple keybinding
(global-set-key (kbd "C-c t") #'user-switch-to-or-open-shell)
(global-set-key (kbd "C-c C-t") #'user-open-shell)

;; Automatically close terminal buffers when the shell process exits
(advice-add 'term-handle-exit :after #'user-handle-terminal-exit)

;; Additional minor modes for terminal mode
(add-hook 'term-mode-hook #'compilation-shell-minor-mode)

;; Optionally load environment variables from the shell
(use-package exec-path-from-shell
  :when user-setting-shell-variables
  :custom
  (exec-path-from-shell-variables user-setting-shell-variables)
  :config
  (exec-path-from-shell-initialize))

;; Multi-window filesystem management
(use-package ranger
  :demand t
  :bind
  (("C-c r" . ranger))
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
  :commands (docker docker-container docker-compose)
  :bind
  ("C-c d d" . docker)
  ("C-c d l" . docker-container)
  ("C-c d c" . docker-compose)
  :custom
  (docker-command user-setting-docker-command))

(provide 'user-operating-system)
;;; user-operating-system.el ends here
