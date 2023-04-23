(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "\\.docker.file\\'" "\\Dockerfile\\'")

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
	       '((c-mode c++-mode)
                 . ("ccls")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'lsp-setup)
