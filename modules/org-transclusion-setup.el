(use-package org-transclusion
  :ensure t
  :bind (("M-s t" . org-transclusion-add))
  :after org
  :config
  (set-face-attribute
   'org-transclusion-fringe nil
   :foreground "gray"
   :background "gray"))

(provide 'org-transclusion-setup)
