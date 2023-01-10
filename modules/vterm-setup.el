(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/bin/bash"))

(use-package multi-vterm
  :bind ("C-c v" . multi-vterm)
  :ensure t)

(provide 'vterm-setup)
