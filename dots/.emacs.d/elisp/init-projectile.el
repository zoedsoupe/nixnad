;;; init-projectile.el --- Sets up Projectile -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for Projectile
;;; Code:

(use-package projectile
  :straight t
  :init
  (projectile-mode)
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :custom
  (projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(provide 'init-projectile)
;;; init-projectile ends here
