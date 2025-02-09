(if (display-graphic-p)
    (progn
      (if (equal system-type 'darwin)
	  (set-frame-font "Liberation Mono 14" nil t)
	(set-frame-font "Liberation Mono 18" nil t))
      (use-package zenburn-theme
	:straight (zenburn-theme :type git :host github :repo "bbatsov/zenburn-emacs")
	:config
	(load-theme 'zenburn t))
      (set-fringe-mode 0))
  (set-face-background 'default "undefined"))

(use-package hide-mode-line
  :straight t)

(provide 'mg-emacs-themes)
