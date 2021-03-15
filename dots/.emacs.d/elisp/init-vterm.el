;;; init0-vterm.el --- Set ups Vterm -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for Vterm
;;; Code:

(use-package vterm
  :straight t
  :commands vterm-mode
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 10000))

(provide 'init-vterm)
;;; init-vterm ends here
