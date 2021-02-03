;;; init-paredit.el --- Sets up paredit -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for paredit
;;; Code:

(use-package paredit
  :ensure t
  :diminish "﹙﹚"
  :init
  (dolist (m (list 'emacs-lisp-mode-hook
		   'lisp-interaction-mode-hook
		   'eval-expression-minibuffer-setup-hook
		   'ielm-mode-hook))
    (add-hook m 'enable-paredit-mode)))

(provide 'init-paredit)
;;; init-paredit ends here
