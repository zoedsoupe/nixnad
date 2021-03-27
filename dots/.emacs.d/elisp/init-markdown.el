;;; init-markdown.el --- Sets up Markdown mode -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for markdown mode
;;; Code:

(defun mdsp/set-markdown-header-font-sizes ()
  "Define header font sizes."
  (dolist (face '((markdown-header-face-1 . 1.2)
                  (markdown-header-face-2 . 1.1)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")

  (defun mdsp/markdown-mode-hook ()
    (mdsp/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'mdsp/markdown-mode-hook))

(provide 'init-markdown)
;;; init-markdown ends here
