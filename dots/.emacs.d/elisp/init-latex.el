;;; init-latex.el  --- Set up LATEX env -*- lexical-binding: t -*-
;;; Commentary:
;;;  Congigure LATEX syntax and pdf exporting
;;; Code:

(use-package pdf-tools
  :straight t
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (add-hook 'pdf-view-mode-hook (lambda ()
				  (bms/pdf-midnite-amber))) ; automatically turns on midnight-mode for pdfs
  )

(use-package auctex-latexmk
  :straight t
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package reftex
  :straight t
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(use-package auto-dictionary
  :straight t
  :ensure t
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
	    (setq TeX-source-correlate-mode t)
	    (setq TeX-source-correlate-method 'synctex)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq-default TeX-master "paper.tex")
	    (setq reftex-plug-into-AUCTeX t)
	    (pdf-tools-install)
	    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		  TeX-source-correlate-start-server t)
	    ;; Update PDF buffers after successful LaTeX runs
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)
	    (add-hook 'LaTeX-mode-hook
		      (lambda ()
			(reftex-mode t)
			(flyspell-mode t)))
	    ))

(provide 'init-latex)
;;; init-latex.el ends here
