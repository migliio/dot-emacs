;; Enable and set org-crypt

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

;; Gpg key to use for encryption

(setq org-crypt-key nil)

(provide 'org-crypt-setup)
