(use-package deft
  :ensure t
  :bind ("C-c d" . deft)
  :config
  ;; Set the deft directory and file extensions
  (setq deft-directory "~/Vault/pkm/pages/")
  (setq deft-extensions '("org" "md" "txt"))
  (add-to-list 'deft-extensions "tex")
  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (setq deft-recursive t))

(provide 'deft-setup)
