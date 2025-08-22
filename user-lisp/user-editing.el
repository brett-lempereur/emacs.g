;;; user-editing -- Text and soruce code editing customisation.

;;; Commentary:

;; Provides common text editing behaviour for all major modes to be as
;; modern and unsurprising as possible.  Most of the customisations in
;; this module are default settings and are overridden for specific
;; programming languages.

;;; Code:

(require 'autorevert)
(require 'winner)

(require 'use-package)

(require 'user-settings)

;; Text encoding
(set-language-environment "UTF-8")

;; Auto-fill
(setopt comment-column 72)
(setopt comment-empty-lines t)
(setopt fill-column 79)

;; Indentation
(setopt indent-tabs-mode nil)
(setopt standard-indent 4)
(setopt tab-always-indent 'complete)
(setopt tab-width 4)

;; Kill ring
(setopt backwards-delete-char-untabify-method 'all)
(setopt kill-do-not-save-duplicates t)
(setopt kill-ring-max 1024)
(setopt save-interprogram-paste-before-kill t)
(setopt yank-pop-change-selection t)

;; Automatically add newlines at the end of a file
(setopt next-line-add-newlines t)

;; Duplication
(setopt duplicate-line-final-position 1)
(setopt duplicate-region-final-position 1)

;; Backups
(setopt make-backup-files nil)

;; Saving
(auto-save-mode -1)
(auto-save-visited-mode t)

;; Fullscreen
(global-set-key (kbd "C-c w") #'toggle-frame-fullscreen)

;; Refresh buffers
(setopt auto-revert-interval 1)
(global-auto-revert-mode)

;; Typing when a region is selected should replace its contents
(delete-selection-mode)

;; When a file is saved delete trailing whitespace and ensure it ends with a
;; newline
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setopt require-final-newline t)

;; Preserve and restore window layouts
(winner-mode t)

;; Case-changing commands should behave sensibly
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)

;; Better buffer menu
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Visual-fill mode should respect the fill-column setting
(use-package visual-fill-column
  :hook
  (visual-line-mode-hook . visual-fill-column-mode))

;; Allow languages to opt-in to hungry-deletion
(use-package hungry-delete
  :commands hungry-delete-mode)

;; Cleanup whitespace at the end of lines when saving
(use-package ws-butler
  :commands ws-butler-mode)

;; Extension and replacement shortcut commands
(use-package crux
  :bind
  ("<M-S-return>" . crux-smart-open-line-above)
  ("<S-return>" . crux-smart-open-line)
  ("C-c b c" . crux-cleanup-buffer-or-region))

;; Use a simple undo and redo system
(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/local/data/undo-tree")))
  :config
  (global-undo-tree-mode))

;; Character and line navigation
(use-package avy
  :bind
  (("C-'" . avy-goto-char-timer)
   ("C-\"" . avy-goto-line))
  :custom
  (avy-all-windows nil)
  (avy-background t)
  (avy-highlight-first t)
  (avy-style 'de-bruijn))

;; Easily navigate to other windows
(use-package ace-window
  :bind
  (("M-o" . ace-window))
  :custom
  (aw-scope 'frame))

;; Multiple cursor editing
(use-package multiple-cursors)

;; Structured editing
(use-package combobulate
  :custom
  (combobulate-key-prefix "C-c o"))

;; Large language model integration
(use-package gptel
  :when user-setting-claude-api-key
  :bind
  ("C-c c ." . gptel)
  ("C-c c c" . gptel-menu)
  ("C-c c f" . gptel-send-file)
  ("C-c c g" . gptel-abort)
  :custom
  (gptel-model user-setting-claude-model)
  :config
  (let ((key (lambda () (getenv user-setting-claude-api-key))))
    (setopt gptel-backend (gptel-make-anthropic "Claude" :stream t :key key))))

(provide 'user-editing)
;;; user-editing.el ends here
