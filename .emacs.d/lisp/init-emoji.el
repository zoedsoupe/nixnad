;;; init-emoji.el --- Sets up emojis -*- lexical-binding: t -*-
;;; Commentary:
;;;  Emojis
;;; Code:

(use-package emojify
  :hook
  (erc-mode . emojify-mode)
  (markdown-mode . emojify-mode)
  (org-mode . emojify-mode)
  :commands emojify-mode)

(provide 'init-emoji)
;;; init-emoji ends here
