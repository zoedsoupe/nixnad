;;; init-org.el --- Sets up Org mode -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for Org mode
;;; Code:

(setq-default fill-column 80)

(defun mdsp/org-mode-setup ()
  "Enables some modes for \"org-mode\" setup."
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :straight t
  :defer t
  :hook (org-mode . mdsp/org-mode-setup)
  :config
  (setq org-ellipsis " ▼ "
	org-directory "~/org/"
	org-agenda-files (list org-directory)
	org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
	;; ex. of org-link-abbrev-alist in action
	;; [[arch-wiki:Name_of_Page][Description]]
	org-link-abbrev-alist    ; This overwrites the default org-link-abbrev-list
	'(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
	  ("youtube" . "https://youtube.com/")
	  ("github" . "https://github.com/"))
        org-cycle-separator-lines 2
	org-todo-keywords        ; This overwrites the default org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "PROJ(p)"           ; A project that contains other tasks
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))  ; Task has been cancelled

  (define-key org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (define-key org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("bh" . "src bash"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("dc" . "src dockerfile"))
(add-to-list 'org-structure-template-alist '("ex" . "src elixir"))
(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(defun mdsp/org-start-presentation ()
  "Start a Org presentation."
  (interactive)
  (org-tree-slide-mode 1)
  (display-line-numbers-mode 0)
  (text-scale-mode 1))

(defun mdsp/org-end-presentation ()
  "End a Org presentation."
  (interactive)
  (display-line-numbers-mode 1)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :straight t
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

(use-package org-bullets
  :straight t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun org-update-cookies-after-save()
  "Update all org cookies on save."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (org-update-statistics-cookies "ALL")))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)))

(provide 'init-org)
;;; init-org ends here
