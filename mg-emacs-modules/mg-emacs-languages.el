(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package dockerfile-mode
  :straight t
  :mode "\\.docker.file\\'" "\\Dockerfile\\'")

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'")

(use-package bpftrace-mode
  :straight t
  :mode "\\.bt\\'")

(use-package racket-mode
  :straight t
  :mode "\\.rkt\\'")

(use-package python-mode
  :straight t
  :mode "\\.py\\'")

(use-package edts
  :straight t)

(use-package erlang-mode
  :ensure nil
  :after (edts)
  :mode "\\.erl\\'")

(use-package nasm-mode
  :straight t)

(use-package gnuplot
  :straight t)

(use-package pyvenv
  :straight t)

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
	       '((c-mode)
		 . ("clangd")))
  (if (eq system-type 'darwin)
      (add-to-list 'eglot-server-programs
		   '((python-mode)
		     . ("/usr/bin/pylsp")))
    (add-to-list 'eglot-server-programs
		 '((python-mode)
		   . ("~/.local/bin/pylsp"))))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'erlang-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

(provide 'mg-emacs-languages)
