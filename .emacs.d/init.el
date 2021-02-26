;;; package --- The core of config -*- lexical-binding: t -*-
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
      '(dashboard company centaur-tabs hl-todo git-gutter-fringe blackout
		  vterm magit gist rainbow-delimiters rainbow-mode projectile
		  paredit flycheck clojure-mode cider clj-refactor flycheck-clj-kondo
		  elixir-mode alchemist exunit flycheck-credo erlang haskell-mode
		  dante attrap use-package org lsp-mode lsp-ui lsp-haskell js2-mode
		  rjsx-mode typescript-mode js2-refactor markdown-mode markdown-toc
		  edit-indirect grip-mode fish-mode company-shell emmet-mode web-mode
		  company-web css-mode less-css-mode sass-mode toml-mode yaml-mode
		  json-mode all-the-icons emojify page-break-lines yasnippet
		  flycheck-popup-tip format-all org-cliplink org-pdftools orgit
		  org-brain org-download centered-window org-tree-slide which-key
		  ob-elixir org-bullets org-roam company-box company-quickhelp bind-key
		  gnu-elpa-keyring-update selectrum orderless consult consult-flycheck
		  evil-nerd-commenter paradox auto-package-update mix yasnippet-snippets
		  dockerfile-mode docker dumb-jump minions mmm-mode))

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

(use-package mmm-mode)
(require 'mmm-defaults)

(setq mmm-global-mode 'auto
      mmm-submode-decoration-level 0
      mmm-parse-when-idle t)

(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . mhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html.leex\\'" . mhtml-mode))

(mmm-add-classes
 '((eex-elixir
    :submode elixir-mode
    :face mmm-declaration-submode-face
    :front "<%[#=%]*" ;; regex to find the opening tag
    :back "%>"))) ;; regex to find the closing tag

(mmm-add-mode-ext-class 'mhtml-mode nil 'eex-elixir)

;; Theme----------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package zerodark-theme)

(load-theme 'omni t)
(omni-setup-modeline-format)

(use-package minions
  :init (minions-mode 1))

(use-package centaur-tabs
  :init
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-icons t)
  (centaur-tabs-height 24)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-style "bar")
  (centaur-tabs-modified-marker "•"))

(centaur-tabs-headline-match)

(use-package all-the-icons)

;; general and bootstrap config
(require 'init-dashboard)
(require 'init-basic)
(require 'init-ibuffer)
(require 'init-key)
(require 'init-todo)
(require 'init-functions)
(require 'init-yas)
(require 'init-flycheck)
(require 'init-git)
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

(setq custom-file (concat user-emacs-directory "/custom.el"))

(provide 'init)
;;; init ends here
