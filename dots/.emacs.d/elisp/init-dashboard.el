;;; init-dashboard.el --- Set up Dashboard -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for emacs-dashboard
;;; Code:

(require 'all-the-icons)

(use-package dashboard
  :straight t
  :functions (all-the-icons-faicon
              all-the-icons-material
              winner-undo
              widget-forward)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("R" . restore-previous-session)
         ("L" . restore-session)
         ("S" . open-custom-file)
         ("q" . quit-dashboard))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :init
  (setq dashboard-startup-banner 'logo
	dashboard-show-shortcuts t
	dashboard-items '((recents  . 10)
			  (agenda . 5)
			  (bookmarks . 5)
			  (projects . 5))

	dashboard-set-init-info t
	dashboard-set-file-icons t
	dashboard-set-heading-icons t
	dashboard-heading-icons '((recents   . "file-text")
				  (bookmarks . "bookmark")
				  (agenda    . "calendar")
				  (projects  . "briefcase")
				  (registers . "database"))

	dashboard-set-navigator t
	dashboard-navigator-buttons
	`(((,(all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0)
	    "Settings" "Open custom file"
	    (lambda (&rest _) (find-file custom-file)))
	   (,(all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
	    "" "Help (?/h)"
	    (lambda (&rest _) (dashboard-hydra/body))
	    font-lock-string-face))))

  (dashboard-setup-startup-hook)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (defun my-dashboard-insert-image-banner (banner)
    "Display an image BANNER."
    (when (file-exists-p banner)
      (let* ((title dashboard-banner-logo-title)
	     (spec (create-image banner))
	     (size (image-size spec))
	     (width (car size))
	     (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
        (goto-char (point-min))
        (insert "\n")
        (insert (make-string left-margin ?\ ))
        (insert-image spec)
        (insert "\n\n")
        (when title
	  (dashboard-center-line title)
	  (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
  (advice-add #'dashboard-insert-image-banner :override #'my-dashboard-insert-image-banner)

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-goto-agenda ()
    "Go to agenda files."
    (interactive)
    (let ((func (local-key-binding "a")))
      (and func (funcall func))))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (let ((func (local-key-binding "p")))
      (and func (funcall func))))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (let ((func (local-key-binding "m")))
      (and func (funcall func))))

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (> (length (window-list-1))
	   1)
        (setq dashboard-recover-layout-p t))

    (delete-other-windows)

    ;; Refresh dashboard buffer
    (when (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)

    ;; Jump to the first section
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
	       (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil))))

(provide 'init-dashboard)
;;; init-dashboard ends here
