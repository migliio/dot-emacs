(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-aspell
  :ensure t)

(global-set-key
 (kbd "C-c s i")
 (lambda ()
   (interactive)
   (ispell-change-dictionary "italiano")))
(global-set-key
 (kbd "C-c s e")
 (lambda ()
   (interactive)
   (ispell-change-dictionary "english")))

(provide 'spellchecking-setup)
