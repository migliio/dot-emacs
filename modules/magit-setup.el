(use-package magit
  :ensure t
  :config
  (setq magit-send-email-workflow t)
  (setq git-commit-fill-column 75))

(require 'git-email)

(provide 'magit-setup)
