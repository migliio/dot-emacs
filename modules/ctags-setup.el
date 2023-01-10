(use-package citre
  :ensure t
  :bind (("C-x c j" . citre-jump)
	 ("C-x c J" . citre-jump-back)
	 ("C-x c p" . citre-ace-peek)
	 ("C-x c u" . citre-update-this-tags-file))
    :config
    (add-to-list 'load-path "~/Repositories/citre")
    (citre-auto-enable-citre-mode-modes '(prog-mode)))

(provide 'ctags-setup)
