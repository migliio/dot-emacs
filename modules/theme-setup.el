(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

(use-package minions
  :ensure t
  :config
    (minions-mode 1))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(when (display-graphic-p)
     (set-frame-font "Monospace 14" nil t))

(provide 'theme-setup)
