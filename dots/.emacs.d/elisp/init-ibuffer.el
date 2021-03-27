;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; IBuffer configurations.
;;
;;; Code:

(use-package ibuffer
  :straight t
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Display icons for buffers
  (use-package all-the-icons-ibuffer
    :straight t
    :init (all-the-icons-ibuffer-mode 1))

  (with-eval-after-load 'counsel
    (with-no-warnings
      (defun my-ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
