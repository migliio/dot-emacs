(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
  (set-frame-font "Liberation Mono 17" nil t)
  (use-package standard-themes :straight t :config
    (load-theme 'standard-dark t)))

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

(set-face-attribute 'mode-line-inactive nil
		    :height 150
		    :background "#665c54"
		    :foreground "#000000"
                    :box '(:width 0))

(set-face-attribute 'mode-line-active nil
		    :height 150
		    :background "#bdae93"
		    :foreground "#000000"
                    :box '(:width 0))

(provide 'mg-emacs-themes)
