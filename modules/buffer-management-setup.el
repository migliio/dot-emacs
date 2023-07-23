(use-package transpose-frame
  :bind (("C-c f t" . transpose-frame)
	 ("C-c f f" . flip-frame)
	 ("C-c f o" . flop-frame))
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

(provide 'buffer-management-setup)
