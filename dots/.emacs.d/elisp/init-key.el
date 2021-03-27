;;; init-key.el --- Sets up Which key -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for which key
;;; Code:

(use-package which-key
  :straight t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  :blackout t)

(provide 'init-key)
;;; init-key ends here
