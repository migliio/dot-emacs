(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package jinx
  :straight t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

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

(use-package gnuplot
  :straight t)

(use-package fasm-mode
  :straight t)

(use-package pyvenv
  :straight t)

(provide 'mg-emacs-languages)
