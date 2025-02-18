;;; user-development -- Common software development configuration.

;;; Commentary:

;; Provides common integrated development environment functionality as
;; well as minor modes shared between multiple programming languages.

;;; Code:

(require 'compile)
(require 'eglot)
(require 'etags)
(require 'eldoc)
(require 'project)
(require 'smerge-mode)
(require 'xref)

(require 'use-package)

(require 'user-settings)

(defun user-projectile-ignore-home (path)
  "Filter PATH from project discovery if it is the home directory."
  (when path
    (let ((expanded-path (expand-file-name path))
          (expanded-home (expand-file-name "~/")))
      (when (not (equal expanded-path expanded-home))
        path))))

(defun user-project-ignore-home (project)
  "Filter PROJECT from project discovery if it is the home directory."
  (when project
    (let ((expanded-path (expand-file-name (nth 2 project)))
          (expanded-home (expand-file-name "~/")))
      (when (not (equal expanded-path expanded-home))
        project))))

;; Function signature assistance
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(setq eldoc-timer 0)

;; Do not prompt to reload tag files
(setq tags-revert-without-query t)

;; Use an easier to type prefix for merge commands
(setopt smerge-command-prefix "C-c m")

;; Major-mode specific indexes
(setopt imenu-auto-rescan t)

;; Filter the home directory from project discovery
(advice-add 'project-try-vc :filter-return #'user-project-ignore-home)

;; Modern compilation buffer behaviour
(setopt compilation-scroll-output t)

;; Cross-referencing behaviour
(setopt xref-auto-jump-to-first-definition 'show)
(setopt xref-auto-jump-to-first-xref 'show)
(setopt xref-history-storage #'xref-window-local-history)
(setopt xref-prompt-for-identifier
        '(not xref-find-definitions xref-find-definitions-other-window
              xref-find-definitions-other-frame xref-find-references))
(setopt xref-truncation-width 100)

;; Additional minor modes for compilation buffers
(add-hook 'compilation-mode-hook #'winnow-mode)

;; Automatically install treesitter grammars
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(python typescript html))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Language server support
(use-package eglot
  :commands eglot
  :bind
  (:map eglot-mode-map
        ("C-," . eglot-code-actions)
        ("C-c l r" . eglot-rename)
        ("C-c l h" . eglot-inlay-hints-mode)
        ("C-c l s" . consult-eglot-symbols))
  :custom
  (eglot-autoshutdown t))

;; Debug adapter protocol support
(use-package dape
  :commands dape
  :hook
  ((dape-compile . kill-buffer))
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-cwd-fn #'projectile-project-root)
  (dape-info-hide-mode-line nil))

;; Syntax checking and linting
(use-package flycheck
  :commands flycheck-mode
  :custom
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-check-syntax-automatically '(idle-buffer-switch idle-change save))
  (flycheck-completing-read-function #'ivy-completing-read)
  (flycheck-display-errors-delay 0.5)
  (flycheck-idle-buffer-switch-delay 0.1)
  (flycheck-idle-change-delay 0.1))

;; Snippet support and automatic completion
(use-package yasnippet
  :commands yas-minor-mode
  :bind
  (("C-c y e" . yas-expand)
   ("C-c y n" . yas-new-snippet)
   ("C-c y ." . yas-next-field-or-maybe-expand)
   ("C-c y ," . yas-prev-field)
   ("C-c y g" . yas-exit-snippet))
  :custom
  (yas-snippet-dirs (list (locate-user-emacs-file "user-data/user-snippets")))
  :config
  (yas-reload-all))

;; Better paranthetical editing support
(use-package smartparens
  :commands smartparens-mode
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-hybrid-kill-entire-symbol t)
  :config
  (require 'smartparens-config))

;; Automatically reindent code while typing
(use-package aggressive-indent
  :commands aggressive-indent-mode)

;; Highlight task, warning, and other comments
(use-package hl-todo
  :commands hl-todo-mode
  :custom
  (hl-todo-require-punctuation t)
  (hl-todo-wrap-movement t))

;; Visually differentiate between nested parentheses
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

;; Distributed revision control interface
(use-package magit
  :demand t
  :hook
  (after-save-hook . magit-after-save-refresh-status)
  :bind
  (("C-c C-g" . magit-dispatch)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame-addition)
   ("C-c g d" . magit-diff-unstaged))
  :custom
  (git-commit-style-convention-checks
   '(non-empty-second-line overlong-summary-line))
  (git-commit-summary-max-length 74)
  (git-commit-use-local-message-ring t)
  (magit-delete-by-moving-to-trash nil)
  (magit-log-auto-more t)
  (magit-no-confirm
   '(set-and-push discard stage-all-changes unstage-all-changes))
  (magit-prefer-push-default t)
  (magit-prefer-remote-upstream t)
  (magit-pull-or-fetch t)
  (magit-repository-directories user-setting-project-paths)
  (magit-save-repository-buffers 'dontask)
  (magit-submodule-remove-trash-gitdirs t)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

;; Highlight revision control changes in the gutter
(use-package diff-hl
  :after magit
  :demand t
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;; Project management
(use-package projectile
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-idle-timer t)
  (projectile-indexing-method user-setting-project-indexing-method)
  (projectile-project-search-path user-setting-project-paths)
  (projectile-sort-order 'recently-active)
  :config
  (advice-add 'projectile-locate-dominating-file
              :filter-return #'user-projectile-ignore-home)
  (projectile-mode))

projectile-tags-command

;; Project search support
(use-package ag
  :hook
  ((ag-mode . winnow-mode))
  :custom
  (ag-context-lines nil)
  (ag-highlight-search t)
  (ag-reuse-window t))

;; Search refinement
(use-package winnow)

;; Git configuration and metadata file support
(use-package git-modes
  :mode
  (("/.gitattributes\\'" . gitattributes-mode)
   ("/.git/info/attributes\\'" . gitattributes-mode)
   ("/git/attributes\\'" . gitattributes-mode)
   ("/.gitmodules\\'" . gitconfig-mode)
   ("/.gitconfig\\'" . gitconfig-mode)
   ("/.git/config\\'" . gitconfig-mode)
   ("/git/config\\'" . gitconfig-mode)
   ("/.gitignore\\'" . gitignore-mode))
  :hook
  ((gitattributes-mode . user-markup-minor-modes)
   (gitconfig-mode . user-markup-minor-modes)
   (gitignore-mode . user-markup-minor-modes)))

;; Indentation highlighting
(use-package highlight-indentation
  :commands (highlight-indentation-mode)
  :custom
  (highlight-indentation-blank-lines t))

;; Highlight the current symbol when idle
(use-package idle-highlight-mode
  :commands idle-highlight-mode
  :custom
  (idle-highlight-idle-time 1))

;; Reliable automatic formatting
(use-package apheleia
  :commands '(apheleia-mode apheleia-format-buffer))

(provide 'user-development)
;;; user-development.el ends here
