(use-package dired
  :ensure nil
  :custom
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-dirs-first t)
  (default-directory "~/"))

(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-files "^\\...+$")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  (dired-omit-files "^\\.[^.].+$")
  :init
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

(use-package openwith
  :straight t
  :config
  (setq openwith-associations '(
				("\\.mp4\\'" "mpv" (file))
				("\\.webm\\'" "mpv" (file))
				("\\.mkv\\'" "mpv" (file))
				("\\.m4a\\'" "mpv --force-window" (file))
				("\\.ppt\\'" "libreoffice" (file))
				("\\.pptx\\'" "libreoffice" (file))
				("\\.doc\\'" "libreoffice" (file))
				("\\.docx\\'" "libreoffice" (file))
				))
  (openwith-mode t))

(use-package pdf-tools
  :straight t
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-links-minor-mode)
  (define-key pdf-view-mode-map (kbd "f") #'pdf-links-isearch-link)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))
  (add-hook 'pdf-view-mode-hook
	    (lambda () (setq header-line-format nil))))

(provide 'mg-emacs-files)
