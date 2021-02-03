;;; init-org.el --- Sets up Org mode -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for Org mode
;;; Code:

(use-package org
  :custom
  (setq org-directory "~/org/"
	org-agenda-files '("~/org/agenda.org")
	org-default-notes-file (expand-file-name "notes.org" org-directory)
	org-ellipsis " ▼ "
	org-log-done 'time
	org-journal-date-format "%B %d, %Y (%A) "
	org-journal-file-format "%Y-%m-%d.org"
	org-hide-emphasis-markers t
	;; ex. of org-link-abbrev-alist in action
	;; [[arch-wiki:Name_of_Page][Description]]
	org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
	'(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
	  ("youtube" . "https://youtube.com/")
	  ("github" . "https://github.com/")) ; TODO (IMPROVE THIS)
	org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
	'((sequence
	   "TODO(t)"           ; A task that is ready to be tackled
	   "BLOG(b)"           ; Blog writing assignments
	   "GYM(g)"            ; Things to accomplish at the gym
	   "PROJ(p)"           ; A project that contains other tasks
	   "VIDEO(v)"          ; Video assignments
	   "WAIT(w)"           ; Something is holding up this task
	   "|"                 ; The pipe necessary to separate "active" states and "inactive" states
	   "DONE(d)"           ; Task has been completed
	   "CANCELLED(c)" )))) ; Task has been cancelled

(defun mdsp/org-start-presentation ()
  "Start a Org presentation."
  (interactive)
  (org-tree-slide-mode 1)
  (display-line-numbers-mode 0)
  (setq text-scale-mode-amount 1.5)
  (text-scale-mode 1))

(defun mdsp/org-end-presentation ()
  "End a Org presentation."
  (interactive)
  (display-line-numbers-mode 1)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :custom
  (setq org-image-actual-width nil)
  :config
  (define-key org-mode-map (kbd "<f8>") 'mdsp/org-start-presentation)
  (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
  (define-key org-tree-slide-mode-map (kbd "q") 'mdsp/org-end-presentation)
  (define-key org-tree-slide-mode-map  (kbd "C->") 'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map  (kbd "C-<") 'org-tree-slide-move-previous-tree)
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message "Presentation started."
        org-tree-slide-deactivate-message "Presentation ended."
        org-tree-slide-header t))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org)
;;; init-org ends here
