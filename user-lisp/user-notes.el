;;; user-notes -- Notetaking and task management environment.

;;; Commentary:

;; This module provides a relatively opinionated setup that defines a
;; general-purpose notebook file and a set of capture templates.

;;; Code:

(require 'org)
(require 'org-capture)

(require 'user-hooks)
(require 'user-settings)

(defun user-notes-find-notebook ()
  "Visit the general-purpose notebook file in a new or existing buffer."
  (interactive)
  (find-file org-default-notes-file))

(defun user-notes-capture-template (name)
  "Return the path of capture of template NAME."
  (locate-user-emacs-file (format "user-data/user-capture-templates/%s" name)))

(defun user-notes-current-archive-file ()
  "Return the path of the current archive file."
  (concat org-directory "Archive/" (format-time-string "%Y") ".org"))

;; Hooks for notebook buffers
(add-hook 'org-mode-hook #'user-markup-minor-modes)

;; Additional modules
(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-tempo)

;; Global keybindings
(global-set-key (kbd "C-c n c") #'org-capture)
(global-set-key (kbd "C-c n a") #'org-agenda)
(global-set-key (kbd "C-c n t") #'org-todo-list)
(global-set-key (kbd "C-c n l") #'org-store-link)
(global-set-key (kbd "C-c n g") #'consult-org-agenda)
(global-set-key (kbd "C-c n f") #'user-notes-find-notebook)

;; Local keybindings
(define-key org-mode-map (kbd "M-g h") #'consult-org-heading)

;; File discovery customisations
(setopt org-directory (file-name-as-directory user-setting-notebook-path))
(setopt org-default-notes-file (concat org-directory "Notebook.org"))
(setopt org-agenda-files (list org-directory))

;; Property customisations
(setopt org-columns-default-format "%20ITEM %TODO %TYPE %3PRIORITY %SIZE %TAGS")
(setopt org-property-format "%-12s %s")

;; Editing customisations
(setopt org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
(setopt org-completion-use-ido t)
(setopt org-hide-leading-stars nil)
(setopt org-return-follows-link t)
(setopt org-special-ctrl-a/e t)
(setopt org-special-ctrl-k t)
(setopt org-startup-indented t)
(setopt org-time-stamp-rounding-minutes '(5 5))

;; Task management customisations
(setopt org-log-done 'time)

;; Performance customisations
(setopt org-fold-core-style 'text-properties)

;; Agenda customisations
(setopt org-agenda-menu-two-columns t)
(setopt org-agenda-ndays 5)
(setopt org-agenda-restore-windows-after-quit t)
(setopt org-agenda-show-outline-path 'title)
(setopt org-agenda-skip-scheduled-if-done t)
(setopt org-agenda-skip-deadline-if-done t)
(setopt org-agenda-start-on-weekday nil)
(setopt org-agenda-todo-list-sublevels nil)

;; Capture customisations
(setq
 org-capture-templates
 `(("t" "Task" (file+olp "" "Tasks")
    (file ,(user-notes-capture-template "task.org")))
   ("h" "Habit" entry (file+olp "" "Habits")
    (file ,(user-notes-capture-template "habit.org")))
   ("n" "Note" entry (file+olp+datetree "" "Notes")
    (file ,(user-notes-capture-template "note.org")))
   ("j" "Journal" entry (file+olp+datetree "" "Journal")
    (file ,(user-notes-capture-template "journal.org")))
   ("m" "Meeting" entry (file+olp+datetree "" "Journal")
    (file ,(user-notes-capture-template "meeting.org")))))

;; Archive customisations
(setopt org-archive-location (user-notes-current-archive-file))

;; Tag customisations
(setopt org-tag-persistent-alist (mapcar #'list user-setting-notebook-projects))

(provide 'user-notes)
;;; user-notes.el ends here
