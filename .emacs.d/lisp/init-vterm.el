;;; init0-vterm.el --- Set ups Vterm -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for Vterm
;;; Code:

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(provide 'init-vterm)
;;; init-vterm ends here
