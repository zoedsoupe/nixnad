;;; init-flucheck.el --- Sets up flycheck -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for flycheck
;;; Code:

(defun +syntax-init-popups-h ()
  "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise."
  (if (and (fboundp 'flycheck-posframe-mode)
             (display-graphic-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1)))


(use-package flycheck
  :straight t
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
  :straight t
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h))

(use-package git-gutter-fringe
  :straight t
  :after flycheck
  :config
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  :init
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(if (fboundp 'fringe-mode) (fringe-mode '4))

(global-git-gutter-mode +1)

(use-package magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-flycheck)
;;; init-flycheck ends here
