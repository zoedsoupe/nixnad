;  _ __ ___   __| |___ _ __
; | '_ ` _ \ / _` / __| '_ \
; | | | | | | (_| \__ \ |_) |
; |_| |_| |_|\__,_|___/ .__/
;                     |_|


;; basic config
(setq user-full-name "Zoey de Souza Pessanha"
      user-mail-address "mdsp@boosting.tech")

(setq org-directory "~/org/")

;; Editor---------------------------------------------------
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq display-line-numbers-type 'relative)
(setq confirm-kill-emacs nil)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
	               kill-buffer-query-functions))

(setq-default indent-tabs-mode nil)

;; ELisp----------------------------------------------------
;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)

;; Dashboard-----------------------------------------------
(use-package dashboard
	     :ensure t
	     :config
	     (dashboard-setup-startup-hook))

;;Flycheck--------------------------------------------------
(use-package flycheck
	     :ensure t
	     :init (global-flycheck-mode))

;; Ivy-----------------------------------------------------
(use-package ivy
	     :ensure t
	     :init
	     (ivy-mode)
	     :custom
	     (ivy-use-virtual-buffers t)
	     (enable-recursive-minibuffers t))

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
             (centaur-tabs-modified-marker "•")
	     :bind
	     ("C-<prior>" . centaur-tabs-backward)
	     ("C-<next>" . centaur-tabs-forward))       

;; Projectile----------------------------------------------
(use-package projectile
	     :init (projectile-mode)
	     :custom
	     (projectile-enable-caching t)
	     :bind-keymap
	     ("C-c p" . projectile-command-map))

;; Company------------------------------------------------
(use-package company
	     :ensure t
	     :hook
	     (after-init . global-company-mode)
	     :custom
	     (company-idle-delay 0.05))

;; Doom-Themes-------------------------------------------
(use-package doom-themes
	     :config
	     (setq doom-themes-enable-bold t
		   doom-themes-enable-italic t)
	     (load-theme 'doom-dracula t)
	     (doom-themes-visual-bell-config)
	     (doom-themes-neotree-config)
	     (doom-themes-org-config))

;; Doom-Modeline---------------------------------------
(use-package doom-modeline
	     :ensure t
	     :init (doom-modeline-mode 1))

;; Hl-TODO--------------------------------------------
(use-package hl-todo
	     :ensure t
	     :init
	     (global-hl-todo-mode))

;; Git-Gutter----------------------------------------
(use-package git-gutter
	     :ensure t
	     :init
	     (global-git-gutter-mode))

;; V-term-------------------------------------------
(use-package vterm
	     :ensure t)

;; LSP-Mode----------------------------------------
(use-package lsp-mode
	     :commands lsp
	     :ensure t
	     :diminish lsp-mode
	     :hook
	     (elixir-mode . lsp)
	     :init
	     (add-to-list 'exec-path "~/elixir_ls/"))

;; Which-key-----------------------------------------
(use-package which-key
	     :ensure t
	     :init (which-key-mode)
	     :diminish which-key-mode
	     :config
	     (setq which-key-idle-delay 0.3))
