(fido-mode t) 
(icomplete-vertical-mode t) 

(use-package projectile
  :ensure t
  :commands projectile-mode projectile-project-name
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path '("~/Repositories"))
  (setq projectile-use-git-grep t)
  (setq projectile-mode-line-prefix " Proj")
  (setq projectile-completion-system 'ido)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Show directories first in dired
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

;; Start searching files from "~"
(setq default-directory "~/")

;; Dired extra
(require 'dired-x)

;; Use ripgrep
(require 'ripgrep-setup)

(provide 'files-navigation-setup)
