(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
<<<<<<< HEAD
  (set-frame-font "Iosevka Comfy 19" nil t)
  (use-package zenburn-theme
    :straight t
    :config
    (load-theme 'zenburn t)))
=======
  (set-frame-font "Liberation Mono 17" nil t)
  (use-package standard-themes :straight t :config
    (load-theme 'standard-dark t)))
>>>>>>> ec85869f6906c72b3452f6861921194d80efe734

(defun mg-init-linux-theme ()
  "Enable the theme enviroment of choice for linux."
  (set-frame-font "Liberation Mono 14" nil t)
  (use-package standard-themes :straight t :config
    (load-theme 'standard-dark t)))

(if (display-graphic-p)
    (progn
      (if (equal system-type 'darwin)
	  (mg-init-macos-theme)
	(mg-init-linux-theme))
      (set-fringe-mode 0))
  (set-face-background 'default "undefined"))

(use-package hide-mode-line
  :straight t)

(provide 'mg-emacs-themes)
