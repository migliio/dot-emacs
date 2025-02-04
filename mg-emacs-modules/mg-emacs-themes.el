(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
  (use-package zenburn-theme
    :straight (zenburn-theme :type git :host github :repo "bbatsov/zenburn-emacs" :branch "master")
    :config
    (load-theme 'zenburn t))
  (set-frame-font "Liberation Mono 18" nil t))

(defun mg-init-linux-theme ()
  "Enable the theme enviroment of choice for linux."
  (set-frame-font "Iosevka Comfy 16" nil t)
  (use-package zenburn-theme
    :straight (zenburn-theme :type git :host github :repo "bbatsov/zenburn-emacs" :branch "master")
    :config
    (load-theme 'zenburn t)))

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
