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
		  flycheck-cask erlang haskell-mode dante attrap use-package org
		  lsp-mode lsp-ui lsp-haskell js2-mode rjsx-mode typescript-mode
		  js2-refactor markdown-mode markdown-toc edit-indirect grip-mode
		  fish-mode company-shell emmet-mode haml-mode pug-mode slim-mode
		  web-mode company-web css-mode less-css-mode sass-mode stylus-mode
		  counsel-css toml-mode yaml-mode json-mode all-the-icons emojify
		  page-break-lines yasnippet flycheck-popup-tip format-all org-cliplink
		  org-pdftools orgit org-brain org-download centered-window org-tree-slide
		  which-key ob-elixir org-bullets org-roam company-prescient company-box
		  company-quickhelp org-tree-slide selectrum orderless consult
		  consult-flycheck))

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
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 3)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t))

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

(provide 'init)
