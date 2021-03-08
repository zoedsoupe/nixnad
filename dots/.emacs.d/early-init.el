;;; early-init.el --- Pre config -*- lexical-binding: t -*-
;;; Commentary:
;;;  Config for early packages
;;; Code:

(defun mdsp--advice-fix-display-graphic-p (func &optional display)
  "Fix `display-graphic-p` so it works while loading the early-init file."
  (if display
      (funcall func display)
    ;; `display-graphic-p' lies by returning nil, but
    ;; `initial-window-system' tells the truth (it is nil only if we
    ;; are actually in a tty environment).
    initial-window-system))

(advice-add #'display-graphic-p :around
	    #'mdsp--advice-fix-display-graphic-p)

(defun mdsp--advice-fix-xw-display-color-p (func &optional display)
  "Fix `xw-display-color-p' so it works while loading the early-init file."
  (if (or display after-init-time)
      (funcall func display)
    ;; Make an educated guess
    initial-window-system))

(advice-add #'xw-display-color-p :around
            #'mdsp--advice-fix-xw-display-color-p)

(defun mdsp--advice-disable-x-resource-application ()
  "Disable `x-apply-session-resources'.
Now, `x-apply-session-resources' normally gets called before
reading the init-file.  However if we do our initialization in the
early init-file, before that function gets called, then it may
override some important things like the cursor color.  So we just
disable it, since there's no real reason to respect X
resources.")

(advice-add #'x-apply-session-resources :override
            #'mdsp--advice-disable-x-resource-application)

;; Load init file
(load
 (expand-file-name "init.el" user-emacs-directory) nil 'nomessage 'nosuffix)

;; Avoid messing with things more than necessary.
(advice-remove #'display-graphic-p #'mdsp--advice-fix-display-graphic-p)
(advice-remove #'xw-display-color-p #'mdsp--advice-fix-xw-display-color-p)

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init ends here
