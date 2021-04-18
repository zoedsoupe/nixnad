;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Zoey de Souza Pessanha"
      user-mail-address "zoey.spessanha@zeetech.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :font "JetBrainsMono" :size 15)
    doom-variable-pitch-font (font-spec :font "FiraCode" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(defun mdsp/get-project-root ()
  "Find the project root using projectile."
    (when (fboundp 'projectile-project-root)
          (projectile-project-root)))

;; COMPLETIONS AND SEARCH----------------------------------------------------------
(use-package! consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . consult-buffer))
  :custom
  (consult-project-root-function #'msdp/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package! orderless
  :custom (completion-styles '(orderless)))

(use-package! selectrum
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ("C-j" . selectrum-next-candidate)
         ("C-k" . selectrum-previous-candidate)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 7)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init
  (selectrum-mode 1))

;; ORG_CONFIGS-------------------------------------------------------------

(defun mdsp/org-start-presentation ()
  "Start a Org presentation."
  (interactive)
  (org-tree-slide-mode 1)
  (display-line-numbers-mode 0)
  (text-scale-mode 2))

(defun mdsp/org-end-presentation ()
  "End a Org presentation."
  (interactive)
  (display-line-numbers-mode 1)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package! org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :custom
  (setq org-image-actual-width nil)
  :config
  (define-key org-mode-map (kbd "<f8>") 'mdsp/org-start-presentation)
  (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
  (define-key org-tree-slide-mode-map (kbd "q") 'mdsp/org-end-presentation)
  (define-key org-tree-slide-mode-map  (kbd "C->") 'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map  (kbd "C-<") 'org-tree-slide-move-previous-tree)
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message "Presentation started."
        org-tree-slide-deactivate-message "Presentation ended."
        org-tree-slide-header t))

(use-package! org
    :config
    (setq org-ellipsis " ▼ "))

(defun org-update-cookies-after-save()
    "Update all org cookies on save."
    (interactive)
    (let ((current-prefix-arg '(4)))
         (org-update-statistics-cookies "ALL")))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)))
