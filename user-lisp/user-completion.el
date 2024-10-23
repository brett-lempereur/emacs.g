;;; user-completion -- Minibuffer and in-buffer completion.

;;; Commentary:

;; Uses lightweight replacements for the built-in incremental narrowing
;; frameworks to provide better minibuffer and in-buffer completion.

;;; Code:

(require 'xref)

(require 'use-package)

(require 'user-development)
(require 'user-helpers)

;; Text and programming language completion
(use-package corfu
  :commands corfu-mode
  :init
  (add-package-subdirectory-to-load-path "corfu" "extensions")
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-margin-formatters (list #'kind-icon-margin-formatter))
  (corfu-max-width 30)
  (corfu-preview-current t)
  (corfu-preselect 'prompt)
  :config
  (require 'corfu-popupinfo)
  (setopt corfu-popupinfo-delay 1.0)
  (corfu-popupinfo-mode))

;; Text and programming language completion icon annotations
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default))

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
   ("C-x r b" . consult-bookmark)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
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
