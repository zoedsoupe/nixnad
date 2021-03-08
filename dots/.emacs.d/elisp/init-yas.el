;;; init-yas.el --- Sets up Yas -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for Yasnipets
;;; Code:


(require 'yasnippet)

(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-verbosity 2)
  (yas-reload-all))

(provide 'init-yas)
;;; init-yas ends here
