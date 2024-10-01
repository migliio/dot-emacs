(use-package ibuffer
  :ensure nil
  :bind
  (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
	(quote (("default"
		 ("org" (mode . org-mode))
		 ("dired" (mode . dired-mode))
		 ("magit" (name . "^magit"))
		 ("c-src" (mode . c-mode))
		 ("python-src" (mode . python-mode))
		 ("virt-manager" (name . "^Virt-manager"))
		 ("brave" (name . "^Brave"))
		 ("jabber" (name . "^*-jabber"))
		 ("vterminal" (name . "^\\*vterminal"))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")))
		 ))))
  (setq ibuffer-default-sorting-mode 'alphabetic)
  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-switch-to-saved-filter-groups "default")
	      )))

(use-package vertico
  :straight t
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(use-package savehist
  :straight t
  :config
  (savehist-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :straight t
  :bind (
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c M-m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)

	 ("C-x b" . consult-buffer)
	 ("C-x r b" . consult-bookmark)
	 ("C-x p b" . consult-project-buffer)

	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)
	 ("C-M-#" . consult-register)

	 ("M-y" . consult-yank-pop)

	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)
	 ("M-g g" . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)

	 ("M-s d" . consult-find)
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)

	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)
	 ("M-s e" . consult-isearch-history)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)

	 :map minibuffer-local-map
	 ("M-s" . consult-history)
	 ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize consult-buffer
		     :preview-key "M-.")
  (setq consult-narrow-key ">"))

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu" :commit "24dccafeea114b1aec7118f2a8405b46aa0051e0")
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(provide 'mg-emacs-buffers-completion)
