(use-package dap-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package racket-mode
  :ensure t
  :config
  (setq geiser-scheme-dir "/usr/share/geiser/"))

(use-package geiser-mit
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
		 (latex-mode . lsp)
		 (haskell-mode . lsp)
		 (python-mode . lsp)
		 (racket-mode . lsp)
		 (c-mode . lsp)
		 ;; if you want which-key integration
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (add-to-list 'exec-path "/home/claudio/.ghcup/hls/")
  (add-to-list 'exec-path "/home/claudio/.local/bin/")
  (add-to-list 'exec-path "/home/claudio/.cargo/bin/"))

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t))

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t)

(provide 'lsp-setup)
