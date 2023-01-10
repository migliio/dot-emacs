(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
