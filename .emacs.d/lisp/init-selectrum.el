;;; init-selectrum.el --- Sets up selectrum -*- lexical-binding: t -*-
;;; Commentary:
;;;  Configuration for selectrum completion
;;; Code:

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless)))

(use-package selectrum
  :straight t
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ("C-j" . selectrum-next-candidate)
         ("C-k" . selectrum-previous-candidate)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 7)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init
  (selectrum-mode 1))

(provide 'init-selectrum)
;;; init-selectrum ends here
