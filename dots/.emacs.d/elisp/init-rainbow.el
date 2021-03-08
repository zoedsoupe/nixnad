;;; init-rainbow.el --- Sets up rainbow -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for rainbow
;;; Code:

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-rainbow)
;;; init-rainbow ends here
