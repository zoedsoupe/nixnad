;;; init-neotree.el --- Sets up Neotree -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for neotree
;;; Code:

(require 'projectile)

(defun my-neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using projectile, find-file-in-project, or the current buffer directory."
  (interactive)
  (require 'neotree)
  (let* ((filepath (buffer-file-name))
         (project-dir
          (with-demoted-errors
              (cond
               ((featurep 'projectile)
                (projectile-project-root))
               (t ;; Fall back to version control root.
                (if filepath
                    (vc-call-backend
                     (vc-responsible-backend filepath) 'root filepath)
                  nil))))))

    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (neotree-show)
      (when project-dir
        (neotree-dir project-dir))
      (when filepath
        (neotree-find filepath)))))

(define-key global-map (kbd "C-c t") 'my-neotree-project-dir-toggle)

(provide 'init-neotree)
;;; init-neotree ends here
