;;; init-emoji.el --- Sets up emojis -*- lexical-binding: t -*-
;;; Commentary:
;;;  Emojis
;;; Code:

(use-package emojify
  :hook (after-init . global-emojify-mode))

(provide 'init-emoji)
;;; init-emoji ends here
