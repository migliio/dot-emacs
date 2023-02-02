(use-package yasnippet
  :ensure t
  :config
  (setq yasnippet-snippets-dir '())
  (setq yas-snippet-dirs
	'("~/.emacs.d/private/cm.snippets"                                              ;; personal snippets
	  "~/Repositories/dot-emacs/elpa/yasnippet-snippets-20220713.1234/snippets"     ;; auto-generated snippets
          ))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(provide 'yasnippet-setup)
