;;; init-git.el --- Sets up git -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for git
;;; Code:

(use-package git-gutter-fringe
  :straight t
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
  (require 'flycheck)
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(if (fboundp 'fringe-mode) (fringe-mode '4))

(provide 'init-git)
;;; init-git ends here
