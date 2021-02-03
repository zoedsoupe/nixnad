;;; init-centaur.el --- Sets up Centaur -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for Centaur tabs
;;; Code:

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-icons t)
  (centaur-tabs-height 24)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-style "bar")
  (centaur-tabs-modified-marker "•"))

(provide 'init-centaur)
;;; init-centaur ends here
