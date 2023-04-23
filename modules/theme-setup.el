(use-package unicode-fonts
  :ensure t
  :after mu4e
  :config (unicode-fonts-setup)(require 'persistent-soft))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (unless (display-graphic-p)
    (load-theme 'doom-gruvbox t)))

(if (display-graphic-p)
    (progn
      (set-frame-font "Inconsolata 18" nil t)
      (set-fringe-mode 0)
      (use-package hc-zenburn-theme :ensure t :config (load-theme 'hc-zenburn t)))
  (set-face-background 'default "undefined"))

(provide 'theme-setup)
