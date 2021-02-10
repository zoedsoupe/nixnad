;;; package --- Summary
;;;
;;; Commentary:
;;;   My packages

;;;  _ __ ___   __| |___ _ __
;;; | '_ ` _ \ / _` / __| '_ \
;;; | | | | | | (_| \__ \ |_) |
;;; |_| |_| |_|\__,_|___/ .__/
;;;                     |_|

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; to load all config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; packages
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stablne" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(defvar my-packages)
(setq my-packages
      '(dashboard company centaur-tabs hl-todo doom-modeline neotree git-gutter-fringe
		  doom-themes vterm magit gist rainbow-delimiters rainbow-mode
		  projectile paredit flycheck clojure-mode cider clj-refactor
		  flycheck-clj-kondo elixir-mode alchemist exunit flycheck-credo
		  erlang haskell-mode dante attrap use-package org lsp-mode lsp-ui
		  lsp-haskell js2-mode rjsx-mode typescript-mode js2-refactor
		  markdown-mode markdown-toc edit-indirect grip-mode fish-mode
		  company-shell emmet-mode web-mode company-web css-mode less-css-mode
		  sass-mode counsel-css toml-mode yaml-mode json-mode all-the-icons
		  emojify page-break-lines yasnippet flycheck-popup-tip format-all
		  org-cliplink org-pdftools orgit org-brain org-download centered-window
		  org-tree-slide which-key ob-elixir org-bullets org-roam company-box
		  company-quickhelp bind-key gnu-elpa-keyring-update selectrum orderless
		  consult consult-flycheck evil-nerd-commenter diminish paradox
		  auto-package-update minions mix yasnippet-snippets dockerfile-mode
		  docker))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; A modern Packages Menu
(use-package paradox
  :init
  (setq paradox-execute-asynchronously t
        paradox-github-token t
        paradox-display-star-count nil)

  ;; Replace default `list-packages'
  (defun my-paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
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

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

;; Doom-Themes-------------------------------------------
(use-package doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-dracula t)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; Doom-Modeline---------------------------------------
(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (minions-mode-line-lighter ""))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 12)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t))

;; Better Commenter----------------------------------------
(use-package evil-nerd-commenter
  :init (evilnc-default-hotkeys t))

;; Remove whitespaces---------------------------------------
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; general and bootstrap config
(require 'init-dashboard)
(require 'init-basic)
(require 'init-ibuffer)
(require 'init-centaur)
(require 'init-key)
(require 'init-todo)
(require 'init-functions)
(require 'init-yas)
(require 'init-flycheck)
(require 'init-selectrum)
(require 'init-consult)
(require 'init-projectile)
(require 'init-rainbow)
(require 'init-neotree)
(require 'init-vterm)
(require 'init-paredit)
(require 'init-company)
(require 'init-lsp)
(require 'init-format)

;; lang-modes
(require 'init-markdown)
(require 'init-org)
(require 'init-emoji)
(require 'init-elixir)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets mix perfect-margin yaml-mode ws-butler which-key web-mode vterm use-package typescript-mode toml-mode stylus-mode slim-mode selectrum sass-mode rjsx-mode rainbow-mode rainbow-delimiters pug-mode paradox orgit org-tree-slide org-roam org-pdftools org-download org-cliplink org-bullets org-brain orderless ob-elixir neotree minions markdown-toc lsp-ui lsp-haskell json-mode js2-refactor ibuffer-projectile hl-todo grip-mode gnu-elpa-keyring-update git-gutter-fringe gist format-all flycheck-popup-tip flycheck-credo flycheck-clj-kondo flycheck-cask fish-mode exunit evil-nerd-commenter erlang emojify emmet-mode edit-indirect doom-themes doom-modeline diminish dashboard dante counsel-css consult-flycheck company-web company-shell company-quickhelp company-prescient company-box clj-refactor centered-window centaur-tabs auto-package-update attrap all-the-icons-ibuffer alchemist)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color nil) :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color nil) :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color nil) :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color nil) :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color nil))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color nil))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color nil))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color nil))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(mode-line ((t (:height 0.85))))
 '(mode-line-inactive ((t (:height 0.85))))
 '(selectrum-current-candidate ((t (:background "#3a3f5a")))))

(provide 'init)
;;; init ends here
