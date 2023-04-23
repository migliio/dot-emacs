(use-package transpose-frame
  :bind (("C-c f t" . transpose-frame)
	 ("C-c f f" . flip-frame)
	 ("C-c f o" . flop-frame))
  :ensure t)

(provide 'buffer-management-setup)
