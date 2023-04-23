(use-package deft
  :ensure t
  :bind ("C-c d" . deft)
  :config
  ;; Set the deft directory and file extensions
  (setq deft-directory "~/Vault/pkm/slip-box/")
  (setq deft-extensions '("org"))
  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (setq deft-recursive t)
  (defun anks-deft-limiting-fn (orig-fun &rest args)
    (let
        ((deft-current-files (-take 30 deft-current-files)))
      (apply orig-fun args)))
  (advice-add 'deft-buffer-setup :around #'anks-deft-limiting-fn))

(provide 'deft-setup)
