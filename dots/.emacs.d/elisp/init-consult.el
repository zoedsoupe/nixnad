;;; init-consult.el --- Sets up consult -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for consult
;;; Code:

(defun mdsp/get-project-root ()
  "Find the project root using projectile."
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . consult-buffer))
  :custom
  (consult-project-root-function #'msdp/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(provide 'init-consult)
;;; init-consult ends here
