;; init-elixir.el --- Initialize elixir configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Elixir configurations.
;;

;;; Code:

(use-package elixir-mode
  :straight t
  :config
  (use-package alchemist
    :straight t
    :hook ((elixir-mode . alchemist-mode)
           (elixir-mode . alchemist-phoenix-mode)))

  (use-package flycheck-credo
    :straight t
    :after flycheck
    :init (flycheck-credo-setup)))

(provide 'init-elixir)
;;; init-elixir.el ends here
