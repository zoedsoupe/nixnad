;;; package --- Summary
;;;
;;; Commentary:
;;;  Packages, editor configs

;;;  _ __ ___   __| |___ _ __
;;; | '_ ` _ \ / _` / __| '_ \
;;; | | | | | | (_| \__ \ |_) |
;;; |_| |_| |_|\__,_|___/ .__/
;;;                     |_|

;;; Code:

;; basic config
(setq user-full-name "Matheus de Souza Pessanha"
      user-mail-address "mdsp@boosting.tech")

;; FUNCTIONS------------------------------------------------
(defun seek-backward-to-char (char)
  "Seek backwards to a character (CHAR)."
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) char))
    (forward-char -1)))

(defun delete-between-pair (char)
  "Delete in between the given pair(CHAR)."
  (interactive "cDelete between char: ")
  (forward-char 1)
  (zap-to-char 1 char)
  (insert char)
  (forward-char -1))

(defvar +format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.
This can be set buffer-locally with `setq-hook!' to disable LSP formatting in
select buffers.")

;;;###autoload
(defvar +popup--display-buffer-alist nil)

;;;###autoload
(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select #'ignore
        :ttl    5)
  "Default properties for popup rules defined with `set-popup-rule!'.")

;;;###autoload
(defun +popup-make-rule (predicate plist)
  "Define popup rules givven a PREDICATE and a PLIST."
  (if (plist-get plist :ignore)
      (list predicate nil)
    (let* ((plist (append plist +popup-defaults))
           (alist
            `((actions       . ,(plist-get plist :actions))
              (side          . ,(plist-get plist :side))
              (size          . ,(plist-get plist :size))
              (window-width  . ,(plist-get plist :width))
              (window-height . ,(plist-get plist :height))
              (slot          . ,(plist-get plist :slot))
              (vslot         . ,(plist-get plist :vslot))))
           (params
            `((ttl      . ,(plist-get plist :ttl))
              (quit     . ,(plist-get plist :quit))
              (select   . ,(plist-get plist :select))
              (modeline . ,(plist-get plist :modeline))
              (autosave . ,(plist-get plist :autosave))
              ,@(plist-get plist :parameters))))
      `(,predicate (+popup-buffer)
                   ,@alist
                   (window-parameters ,@params)))))

;;;###autodef
(defun set-popup-rule! (predicate &rest plist)
  "Define a popup rule.
These rules affect buffers displayed with `pop-to-buffer' and `display-buffer'
\(or their siblings).  Buffers displayed with `switch-to-buffer' (and its
variants) will not be affected by these rules (as they are unaffected by
`display-buffer-alist', which powers the popup management system).
PREDICATE can be either a) a regexp string (matched against the buffer's name)
or b) a function that takes two arguments (a buffer name and the ACTION argument
of `display-buffer') and returns a boolean.
PLIST can be made up of any of the following properties:
:ignore BOOL
  If BOOL is non-nil, popups matching PREDICATE will not be handled by the popup
  system.  Use this for buffers that have their own window management system like
  magit or helm.
:actions ACTIONS
  ACTIONS is a list of functions or an alist containing (FUNCTION . ALIST).  See
  `display-buffer''s second argument for more information on its format and what
  it accepts.  If omitted, `+popup-default-display-buffer-actions' is used.
:side 'bottom|'top|'left|'right
  Which side of the frame to open the popup on.  This is only respected if
  `+popup-display-buffer-stacked-side-window-fn' or `display-buffer-in-side-window'
  is in :actions or `+popup-default-display-buffer-actions'.
:size/:width/:height FLOAT|INT|FN
  Determines the size of the popup.  If more than one of these size properties are
  given :size always takes precedence, and is mapped with 'window-width' or
  'window-height' depending on what :side the popup is opened.  Setting a height
  for a popup that opens on the left or right is harmless, but comes into play
  if two popups occupy the same :vslot.
  If a FLOAT (0 < x < 1), the number represents how much of the window will be
    consumed by the popup (a percentage).
  If an INT, the number determines the size in lines (height) or units of
    character width (width).
  If a function, it takes one argument: the popup window, and can do whatever it
    wants with it, typically resize it, like `+popup-shrink-to-fit'.
:slot/:vslot INT
  (This only applies to popups with a :side and only if :actions is blank or
  contains the `+popup-display-buffer-stacked-side-window-fn' action) These control
  how multiple popups are laid out.  INT can be any integer, positive and
  negative.
  :slot controls lateral positioning (e.g. the horizontal positioning for
    top/bottom popups, or vertical positioning for left/right popups).
  :vslot controls popup stacking (from the edge of the frame toward the center).
  Let's assume popup A and B are opened with :side 'bottom, in that order.
    If they possess the same :slot and :vslot, popup B will replace popup A.
    If popup B has a higher :slot, it will open to the right of popup A.
    If popup B has a lower :slot, it will open to the left of popup A.
    If popup B has a higher :vslot, it will open above popup A.
    If popup B has a lower :vslot, it will open below popup A.
:ttl INT|BOOL|FN
  Stands for time-to-live.  It can be t, an integer, nil or a function.  This
  controls how (and if) the popup system will clean up after the popup.
  If any non-zero integer, wait that many seconds before killing the buffer (and
    any associated processes).
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed and is left to its own devices.
  If t, resort to the default :ttl in `+popup-defaults'.  If none exists, this is
    the same as nil.
  If a function, it takes one argument: the target popup buffer.  The popup
    system does nothing else and ignores the function's return value.
:quit FN|BOOL|'other|'current
  Can be t, 'other, 'current, nil, or a function.  This determines the behavior
  of the exit keys in or outside of popup windows.
  If t, close the popup if exit is pressed anywhere.
  If 'other, close this popup if exit is pressed outside of any popup.  This
    is great for popups you may press exit a lot in.
  If 'current, close the current popup if exit is pressed from inside of the
    popup.  This makes it harder to accidentally close a popup until you really
    want to.
  If nil, pressing exit will never close this popup.
  If a function, it takes one argument: the to-be-closed popup window, and is
    run when exit is pressed while that popup is open.  It must return one of
    the other values to determine the fate of the popup.
:select BOOL|FN
  Can be a boolean or function.  The boolean determines whether to focus the
  popup window after it opens (non-nil) or focus the origin window (nil).
  If a function, it takes two arguments: the popup window and originating window
    (where you were before the popup opened).  The popup system does nothing else
    and ignores the function's return value.
:modeline BOOL|FN|LIST
  Can be t (show the default modeline), nil (show no modeline), a function that
  returns a modeline format or a valid value for `mode-line-format' to be used
  verbatim.  The function takes no arguments and is run in the context of the
  popup buffer.
:autosave BOOL|FN
  This parameter determines what to do with modified buffers when closing popup
  windows.  It accepts t, 'ignore, a function or nil.
  If t, no prompts.  Just save them automatically (if they're file-visiting
    buffers).  Same as 'ignore for non-file-visiting buffers.
  If nil (the default), prompt the user what to do if the buffer is
    file-visiting and modified.
  If 'ignore, no prompts, no saving.  Just silently kill it.
  If a function, it is run with one argument: the popup buffer, and must return
    non-nil to save or nil to do nothing (but no prompts).
:parameters ALIST
  An alist of custom window parameters.  See `(elisp)Window Parameters'.
If any of these are omitted, defaults derived from `+popup-defaults' will be
used.
\(fn PREDICATE &key IGNORE ACTIONS SIDE SIZE WIDTH HEIGHT SLOT VSLOT TTL QUIT SELECT MODELINE AUTOSAVE PARAMETERS)"
  (declare (indent defun))
  (push (+popup-make-rule predicate plist) +popup--display-buffer-alist)
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

;;;###autoload
(defun +syntax-init-popups-h ()
  "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
  (unless (and (bound-and-true-p lsp-ui-mode)
               lsp-ui-sideline-enable)
    (if (and (fboundp 'flycheck-posframe-mode)
             (display-graphic-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1))))

;;;###autoload
(defun +format/buffer ()
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive)
  (if (eq major-mode 'org-mode)
      (when (org-in-src-block-p t)
        (+format--org-region nil nil))
    (call-interactively
     (cond ((and +format-with-lsp
                 (bound-and-true-p lsp-mode)
                 (lsp-feature? "textDocument/formatting"))
            #'lsp-format-buffer)
           (#'format-all-buffer)))))

;;;###autoload
(defun +format/region (beg end)
  "Run the active formatter on the lines within BEG and END.
WARNING: this may not work everywhere.  It will throw errors if the region
contains a syntax error in isolation.  It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (if (and (eq major-mode 'org-mode)
           (org-in-src-block-p t))
      (+format--org-region beg end)
    (cond ((and +format-with-lsp
                (bound-and-true-p lsp-mode)
                (lsp-feature? "textDocument/rangeFormatting"))
           (call-interactively #'lsp-format-region))
          ((save-restriction
             (narrow-to-region beg end)
             (let ((+format-region-p t))
               (+format/buffer)))))))

;;;###autoload
(defun +format-enable-on-save-h ()
  "Enables formatting on save."
  (add-hook 'before-save-hook #'+format-buffer-h nil t))

;;;###autoload
(defalias '+format-buffer-h #'+format/buffer
  "Format the source code in the current buffer with minimal feedback.")

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
;; Dashboard-----------------------------------------------
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-center-content t
        dashboard-banner-logo-title "Welcome to Emacs Dashboard"
        dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-set-file-icons t))

;;Flycheck--------------------------------------------------
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :commands flycheck-list-errors flycheck-buffer
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25))

(use-package flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h))

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
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t)
  :after yasnippet
  :hook
  (yas-prompt-functions . +ivy-yas-prompt-fn))

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
  (dolist (m (list 'emacs-lisp-mode-hook
		   'lisp-interaction-mode-hook
		   'eval-expression-minibuffer-setup-hook
		   'ielm-mode-hook))
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
  :config
  (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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
        company-auto-commit nil
        company-auto-commit-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  (eldoc-add-command 'company-complete-selection
                     'company-complete-common
                     'company-abort)
  :after eldoc)


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
  (doom-modeline-bar-width 3)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t))

;; Hl-TODO--------------------------------------------
(use-package hl-todo
  :init (global-hl-todo-mode))

;; Git-Gutter----------------------------------------
(use-package git-gutter-fringe
  :config
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  :after flycheck
  :init
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(if (fboundp 'fringe-mode) (fringe-mode '4))

(global-git-gutter-mode +1)
;; LSP-Mode----------------------------------------
(defvar +lsp-company-backends 'company-capf
  "The backends to prepend to `company-backends' in `lsp-mode' buffers.
Can be a list of backends; accepts any value `company-backends' accepts.")

(use-package lsp-mode
  :commands lsp-install-server
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  (haskell-mode . lsp)
  (typescript-mode . lsp)
  (clojure-mode . lsp)
  (erlang-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keep-workspace-alive nil
	lsp-enable-folding nil
	lsp-enable-text-document-color nil
	lsp-enable-on-type-formatting nil
	lsp-headerline-breadcrumb-enable nil
	lsp-keymap-prefix "C-c l")
  (add-to-list 'exec-path "~/elixir_ls/")
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :config
  (dolist (dir '("[/\\\\]_build"
		 "[/\\\\]deps"
		 "[/\\\\]node_modules"
		 "[/\\\\]\\.cache"))
    (push dir lsp-file-watch-ignored-directories))
  (set-popup-rule! "^\\*lsp-help" :size 0.35 :quit t :select t)
  (defadvice +lsp--respect-user-defined-checkers-a (orig-fn &rest args)
    "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'."
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply orig-fn args)
          (setq-local flycheck-checker old-checker))
      (apply orig-fn args)))

  (add-hook 'lsp-mode-hook
	    (defun +lsp-display-guessed-project-root-h ()
	      "Log what LSP things is the root of the current project."
	      ;; Makes it easier to detect root resolution issues.
	      (when-let (path (buffer-file-name (buffer-base-buffer)))
		(if-let (root (lsp--calculate-root (lsp-session) path))
		    (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
		  (lsp--info "Could not guess project root."))))
	    #'+lsp-optimization-mode)

  (add-hook 'lsp-completion-mode-hook
	    (defun +lsp-init-company-backends-h ()
	      (when lsp-completion-mode
		(set (make-local-variable 'company-backends)
		     (cons +lsp-company-backends
			   (remove +lsp-company-backends
				   (remq 'company-capf company-backends)))))))
  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice +lsp-defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (prog1 (funcall orig-fn restart)
          (+lsp-optimization-mode -1))
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspace)
                   (with-lsp-workspace workspace
                     (unless (lsp--workspace-buffers workspace)
                       (let ((lsp-restart 'ignore))
                         (funcall orig-fn))
                       (+lsp-optimization-mode -1))))
             lsp--cur-workspace)))))

;; LSP-UI----------------------------------------------
(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
	lsp-ui-sideline-show-hover nil
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-max-height 8
	lsp-ui-doc-max-width 35
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-enable nil))

;; DAP-mode--------------------------------------------
(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

;; Markdown-mode---------------------------------------
(use-package markdown-mode
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
(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

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

;; Dante-------------------------------------------------------
(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flymake-mode)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  (add-hook 'haskell-mode-hook 'dante-mode))

(provide 'config)
;;; config.el ends here
