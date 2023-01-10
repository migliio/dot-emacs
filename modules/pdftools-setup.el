(use-package pdf-tools
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))
  (add-hook 'pdf-view-mode-hook
	    (lambda () (setq header-line-format nil))))

(provide 'pdftools-setup)
