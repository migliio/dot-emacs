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

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
	      (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
	      nil)
            nil
            t))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-links nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook (((latex-mode haskell-mode python-mode racket-mode c-mode) . lsp-deferred)
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq c-default-style "linux")
  (add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
  (add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
  (add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-clangd-binary-path "/nix/store/0m2gx5r8k4ngpv0g6iw3ylq3ypcr14kp-clang-11.1.0/bin/clangd")
  (add-to-list 'exec-path "/home/claudio/.ghcup/hls/")
  (add-to-list 'exec-path "/home/claudio/.local/bin/")
  (add-to-list 'exec-path "/home/claudio/.cargo/bin/"))

(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package dap-mode
  :ensure t)

(provide 'lsp-setup)
