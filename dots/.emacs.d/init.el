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
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; packages
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stablne" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(defvar my-packages)
(setq my-packages
      '(dashboard centaur-tabs hl-todo git-gutter-fringe blackout
		  vterm magit gist rainbow-delimiters rainbow-mode projectile
		  paredit flycheck elixir-mode alchemist exunit flycheck-credo
		  erlang haskell-mode attrap use-package org js2-mode rjsx-mode
		  typescript-mode js2-refactor markdown-mode markdown-toc grip-mode
		  edit-indirect fish-mode emmet-mode web-mode css-mode less-css-mode
		  sass-mode toml-mode yaml-mode json-mode all-the-icons emojify
		  page-break-lines yasnippet flycheck-popup-tip format-all
		  org-cliplink org-pdftools orgit org-brain org-download
		  centered-window org-tree-slide which-key ob-elixir org-bullets
		  bind-key gnu-elpa-keyring-update selectrum orderless
		  consult consult-flycheck evil-nerd-commenter paradox mix
		  yasnippet-snippets dumb-jump minions mmm-mode org
		  org-wild-notifier package-lint pdf-tools auctex-latexmk
		  reftex auto-dictionary))

;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install packages
(dolist (package my-packages)
  (straight-use-package package))

(use-package mmm-mode
  :straight t)
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

(load-theme 'omni t)
(omni-setup-modeline-format)

(use-package minions
  :straight t
  :init (minions-mode 1))

(use-package centaur-tabs
  :straight t
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

(use-package org-wild-notifier
  :straight t
  :init (org-wild-notifier-mode))

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
(require 'init-format)

;; lang-modes
(require 'init-markdown)
(require 'init-org)
(require 'init-emoji)
(require 'init-elixir)

(setq custom-file (concat user-emacs-directory "/custom.el"))

(provide 'init)
;;; init ends here
