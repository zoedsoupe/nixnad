;; init-basic.el --- Better default configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Better defaults.
;;
;;; Code:

;; BASIC CONFIG---------------------------------------------
(setq user-full-name "Zoey de Souza Pessanha"
      user-mail-address "mdsp@boosting.tech")

;; Editor---------------------------------------------------
(menu-bar-mode -1) ; disable menu bar
(toggle-scroll-bar -1) ; disable scroll bar
(tool-bar-mode -1) ; disable tool bar
(set-fringe-mode 10)
(setq visible-bell nil) ; disable visual bell
(setq indent-tabs-mode nil) ; changes from tabs to spaces

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq confirm-kill-emacs nil) ; disable confirmation to exit emacs
(setq inhibit-splash-screen t) ; disable splash screen
(fset 'yes-or-no-p 'y-or-n-p) ; change all "yes" questions to "y"

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions)) ; disable confirmation on killing buffer

(set-face-attribute 'default nil :font "VictorMono Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil
                    :font "UbuntuCondensed Nerd Font"
                    :height 120)
(set-face-attribute 'variable-pitch nil
                    :font "Hasklug Nerd Font"
                    :height 120
                    :weight 'regular)

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

(use-package recentf
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

(provide 'init-basic)
;;; init-basic ends here
