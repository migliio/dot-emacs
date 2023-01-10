(use-package org-remark
  :ensure t
  :defer t
  :bind (("C-c n m" . org-remark-mark)
	 ("C-c n o" . org-remark-open)
	 ("C-c n ]" . org-remark-view-next)
	 ("C-c n [" . org-remark-view-prev)
	 ("C-c n r" . org-remark-remove))
  :config
  (org-remark-global-tracking-mode +1))

(provide 'org-remark-setup)
