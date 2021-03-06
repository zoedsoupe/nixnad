;;; init-docker.el --- Sets up docker _*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for docker mode
;;; Code:

(use-package docker
  :bind ("C-c d" . docker))

(provide 'init-docker)
;;; init-docker ends here
