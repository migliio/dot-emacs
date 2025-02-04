(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq c-default-style "linux")

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
		 . ("clangd"
                    "-j=8"
                    "--log=error"
                    "--malloc-trim"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0")))
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

(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (add-to-list 'exec-path "/Users/gli/.cargo/bin/")
  (eglot-booster-mode))

(provide 'mg-emacs-languages)
