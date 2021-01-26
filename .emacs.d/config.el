;  _ __ ___   __| |___ _ __
; | '_ ` _ \ / _` / __| '_ \
; | | | | | | (_| \__ \ |_) |
; |_| |_| |_|\__,_|___/ .__/
;                     |_|


;; basic config
(setq user-full-name "Matheus de Souza Pessanha"
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
                              ;
(setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
	      kill-buffer-query-functions)) ; disable confirmation on killing buffer

(set-face-attribute 'default nil :font "Victor Mono Medium" :height 170)
(set-face-attribute 'fixed-pitch nil
                    :font "Ubuntu Condendsed"
                    :height 170)
(set-face-attribute 'variable-pitch nil
                    :font "Hasklug"
                    :height 170
                    :weight 'regular)

;; Dashboard-----------------------------------------------
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-center-content t
        dashboard-banner-logo-title "Welcome to Emacs Dashboard"
        dashboard-startup-banner 'logo
        dashboard-set-headings-icons t
        dashboard-set-file-icons t))

;;Flycheck--------------------------------------------------
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode)
  :commmands flycheck-list-errors flycheck-buffer
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25))

(use-package flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (setq flycheck-popup-tip-error-prefix "X "))

;; Ivy-----------------------------------------------------
(use-package ivy
  :diminish
  :init
  (ivy-mode)
  :custom
  (enable-recursive-minibuffers t)
  :bind
  ("C-s" . swiper)
  :config
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-format-function #'ivy-read-action-format-columns
        projectile-completion-system 'ivy
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t)
  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-line-or-arrow)
  (after! yasnippet
    (add-hook 'yas-prompt-functions #'+ivy-yas-prompt-fn)))

;; Yasnippets----------------------------------------------
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; Rainbow-delimiters--------------------------------------
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; V-term---------------------------------------------------
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; Paredit--------------------------------------------------
(use-package paredit
  :ensure t
  :diminish "﹙﹚"
  :init
  (dolist (m (list 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook 'eval-expression-minibuffer-setup-hook 'ielm-mode-hook))
    (add-hook m 'enable-paredit-mode)))

;; Centaur Tabs---------------------------------------------
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-icons t)
  (centaur-tabs-height 24)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-style "bar")
  (centaur-tabs-modified-marker "•"))

;; Projectile----------------------------------------------
(use-package projectile
  :init (projectile-mode)
  :custom
  (projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))
nnn
;; Company------------------------------------------------
(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.05)
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)
        company-backends '(company-capf)
        company-auto-complete nil
        company-auto-commit-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  :config
  (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)
  (after! eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-abort)))


;; Doom-Themes-------------------------------------------
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; Doom-Modeline---------------------------------------
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

;; Hl-TODO--------------------------------------------
(use-package hl-todo
  :init (global-hl-todo-mode))

;; Git-Gutter----------------------------------------
(use-package git-gutter-fringe
  :if (fboundp 'fringe-mode) (fringe-mode '4)
  :config
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(after! flycheck
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

;; LSP-Mode----------------------------------------
(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  (haskell-mode . lsp)
  (typescript-mode . lsp)
  (clojure-mode . lsp)
  (erlang-mode . lsp)
  :init
  (add-to-list 'exec-path "~/elixir_ls/")
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point)))

;; LSP-UI----------------------------------------------
(use-package lsp-ui
  :straight t
  :hook (lsp-mode .lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; DAP-mode--------------------------------------------
(use-package dap-mode
  :straight t
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

;; Markdown-mode---------------------------------------
(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun mdsp/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun mdsp/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'mdsp/markdown-mode-hook))

;; Which-key-----------------------------------------
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Emojify---------------------------------------------
(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; Magit-----------------------------------------------
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Format-all---------------------------------------------
(defvar +format-on-save-enabled-modes
  '(not emacs-lisp-mode))
(defvar +format-preserve-identation t)
(defvar-local +format-with nil)
(defvar +format-with-lsp t)

(defun +format-enable-on-save-maybe-h ()
  "Enable formatting on save in  certain major modes."
  (unless (or (eq major-mode 'fundamental-mode)
              (cond ((booleanp +format-on-save-enabled-modes)
                     (null +format-on-save-enabled-modes))
                    ((eq (car +format-on-save-enabled-modes) 'not)
                     (memq major-mode (cdr +format-on-save-enabled-modes)))
                    ((not (memq major-mode +format-on-save-enabled-modes))))
              (not (require 'format-all nil t)))
    (+format-enable-on-save-h)))

(advice-add #'format-all--probe :around #'format-probe-a)
(advice-add #'format-all-buffer--with :around #'+format-buffer-a)
(add-to-list 'debug-ignored-errors "^Don't know how to format ")

(provide 'config)
;;; config.el ends here
