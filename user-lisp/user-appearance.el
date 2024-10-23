;;; user-appearance -- Appearance customisations.

;;; Commentary:

;; Provides a minimal and modern set of appearance customisations.

;;; Code:

(require 'use-package)

(require 'user-settings)

;; Hide messages when starting a new session
(setopt inhibit-splash-screen t)
(setopt inhibit-startup-message t)

;; Free fortune cookie with each new session
(let* ((fortune (cookie user-setting-cookie-file))
       (formatted (replace-regexp-in-string "\\`\\|\n" "\\&;; " fortune)))
  (setopt initial-scratch-message (concat formatted "\n\n")))

;; Support the mouse in the console
(xterm-mouse-mode)

;; Don't blink the cursor
(blink-cursor-mode 0)

;; Do not beep
(setq ring-bell-function 'ignore)

;; Always enable font lock mode
(global-font-lock-mode)

;; Show column and line numbers in the mode line
(column-number-mode)
(line-number-mode)

;; Show the current time in the mode line but do not show system load
(setopt display-time-format "%H:%M")
(setopt display-time-load-average-threshold 1000)
(display-time-mode)

;; Use a better font in graphical frames, we select the first available font
;; from user settings so that the default configuration is less likely to fail
;; on new machines
(when (display-graphic-p)
  (let* ((fonts (mapcar (lambda (f)
                          (font-spec :family (car f) :size (cdr f)))
                        user-setting-font))
         (available-font (seq-find #'find-font fonts)))
    (if available-font
        (set-frame-font available-font nil t t)
      (warn "No available font matches user configuration, using default")))
  (setopt line-spacing user-setting-line-spacing))

;; Use a custom theme in graphical frames
(when (display-graphic-p)
  (when user-setting-theme-package
    (require user-setting-theme-package))
  (load-theme user-setting-theme t))

;; Disable frame decorations
(menu-bar-mode user-setting-menu-bar-mode)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Do not use system tooltips when available
(when (eq window-system 'x)
  (setopt x-gtk-use-system-tooltips nil))

;; Increase the default graphical frame size
(when (display-graphic-p)
  (set-frame-size (selected-frame) 85 42))

;; Show smaller gutter fringes
(when (display-graphic-p)
  (fringe-mode '(10 . 10)))

;; Customisation appearance
(setopt custom-buffer-done-kill t)
(setopt custom-variable-default-form 'lisp)

;; Improve the appearance of the modeline
(use-package spaceline
  :custom
  (powerline-default-separator 'nil)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-flycheck-info-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-purpose-off)
  (spaceline-toggle-hud-off))

;; Allow rendering images
(use-package svg-lib)

(provide 'user-appearance)
;;; user-appearance.el ends here
