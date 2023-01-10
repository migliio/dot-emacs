(use-package org-noter
  :bind ("C-c r" . org-noter)
  :ensure t
  :config
  (setq org-noter-auto-save-last-location t))

(provide 'org-noter-setup)
