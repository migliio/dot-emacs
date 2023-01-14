(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(set-frame-font "JetBrains Mono NL 14" nil t)

(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(92 92))
  (add-to-list 'default-frame-alist '(alpha 92 92)))

(provide 'theme-setup)
