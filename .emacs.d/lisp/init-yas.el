;;; init-yas.el --- Sets up Yas -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for Yasnipets
;;; Code:


(require 'yasnippet)

(use-package yasnippet
  :hook (prog-mode . yas-global-mode)
  :config
  (yas-reload-all))

(provide 'init-yas)
;;; init-yas ends here
