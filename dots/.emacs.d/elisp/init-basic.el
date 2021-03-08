;; init-basic.el --- Better default configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Better defaults.
;;
;;; Code:

;; BASIC CONFIG---------------------------------------------
(setq user-full-name "Matheus de Souza Pessanha"
      user-mail-address "mdsp@boosting.tech")

;; Editor---------------------------------------------------
(menu-bar-mode -1) ; disable menu bar
(toggle-scroll-bar -1) ; disable scroll bar
(tool-bar-mode -1) ; disable tool bar
(set-fringe-mode 10)
(setq visible-bell nil) ; disable visual bell
(setq indent-tabs-mode nil) ; changes from tabs to spaces

(setq backup-directory-alist
            `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
            `((".*" ,temporary-file-directory t)))

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1)
		   (setq display-line-numbers 'relative))))

(setq confirm-kill-emacs nil) ; disable confirmation to exit emacs
(setq inhibit-splash-screen t) ; disable splash screen
(fset 'yes-or-no-p 'y-or-n-p) ; change all "yes" questions to "y"

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions)) ; disable confirmation on killing buffer

(set-face-attribute 'default nil :font "JetBrains Mono Medium Medium Nerd Font Complete Mono" :height 120)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono Medium Medium Nerd Font Complete"
                    :height 120)
(set-face-attribute 'variable-pitch nil
                    :font "Victor Mono Regular Nerd Font Complete"
                    :height 120
                    :weight 'regular)

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Basic Packages------------------------------------

;; Required by `use-package'
(use-package bind-key
  :straight t)
(use-package blackout
  :straight t
  :demand t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update
  :straight t)

;; Replace default `list-packages'
(defun my-paradox-enable (&rest _)
  "Enable paradox, overriding the default package-menu."
  (paradox-enable))

;; A modern Packages Menu
(use-package paradox
  :straight t
  :init
  (setq paradox-execute-asynchronously t
        paradox-github-token t
        paradox-display-star-count nil)
  (advice-add #'list-packages :before #'my-paradox-enable)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                (let ((buf (get-buffer-create "*Paradox Report*"))
                      (inhibit-read-only t))
                  (with-current-buffer buf
                    (page-break-lines-mode 1))))
              t)))

(use-package recentf
  :straight t
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Better Commenter----------------------------------------
(use-package evil-nerd-commenter
  :straight t
  :init (evilnc-default-hotkeys t))

;; Remove whitespaces---------------------------------------
(use-package ws-butler
  :straight t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :straight t
  :bind (([remap query-replace] . #'vr/query-replace)))

;; Editing---------------------------------------------
;;; Text formatting

(add-to-list 'safe-local-variable-values '(auto-fill-function . nil))

(add-to-list 'safe-local-eval-forms '(visual-line-mode +1))

;; When region is active, make `capitalize-word' and friends act on
;; it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

(provide 'init-basic)
;;; init-basic ends here
