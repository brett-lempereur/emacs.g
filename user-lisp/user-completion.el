;;; user-completion -- Minibuffer and in-buffer completion.

;;; Commentary:

;; Uses lightweight replacements for the built-in incremental narrowing
;; frameworks to provide better minibuffer and in-buffer completion.

;;; Code:

(require 'xref)

(require 'use-package)

(require 'user-development)

;; Text and programming language completion
(use-package corfu
  :commands corfu-mode
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current t)
  (corfu-preselect 'prompt))

;; Better incremental narrowing
(use-package vertico
  :custom
  (vertico-scroll-margin 3)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode))

;; Better incremental narrowing completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Inline documentation for incremental narrowing
(use-package marginalia
  :config
  (marginalia-mode))

;; Practical incremental narrowing commands
(use-package consult
  :demand t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-g i" . consult-imenu)
   ("M-g d" . consult-flymake))
  :config
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

;; Contextual actions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :custom
  (embark-indicators '(embark-mixed-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-mixed-indicator-delay 1)
  (embark-prompter #'embark-keymap-prompter))

;; Contextual actions completion integration
(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;; Incremental narrowing of workspace symbols
(use-package consult-eglot
  :commands consult-eglot-symbols)

;; Interactive guided keyboard command completion
(use-package which-key
  :demand t
  :bind
  (("C-c C-k k" . which-key-show-top-level)
   ("C-c C-k ," . which-key-show-major-mode)
   ("C-c C-k ." . which-key-show-minor-mode-keymap))
  :config
  (which-key-mode))

(provide 'user-completion)
;;; user-completion.el ends here
