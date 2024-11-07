(when (and (display-graphic-p) (not (eq system-type 'darwin)))
  (defun mg-exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))
  (use-package exwm
    :straight t
    :config

    ;; Custom tools and applications that I use
    (defconst mg-browser "/usr/bin/firefox")
    (defconst mg-keyboard-layout-changer "/usr/bin/setxkbmap")

    ;; Custom application started that leverages on `xstarter'
    (defun mg-starter ()
      "Choose the application to run from a list generated with
`xstarter', which is, then, a prerequisite for this function to
work."
      (interactive)
      (let* ((candidates (split-string
			  (shell-command-to-string "xstarter -P")
			  "\n"
			  t))
	     (application-path (completing-read
				"Application to launch: "
				candidates)))
	(start-process "" nil application-path)))

    (setq exwm-workspace-number 6)
    (add-hook 'exwm-update-class-hook #'mg-exwm-update-class)
    (require 'exwm-randr)
    (exwm-randr-enable)
    (setq exwm-input-prefix-keys
	  '(?\C-x
	    ?\C-u
	    ?\C-n
	    ?\C-t
	    ?\C-h
	    ?\C-p
	    ?\C-g
	    ?\M-x
	    ?\M-`
	    ?\M-&
	    ?\M-:
	    ?\C-\M-j
	    ?\C-\ ))
    (setq exwm-input-simulation-keys
	  '(([?\C-b] . [left])
	    ([?\C-f] . [right])
	    ([?\C-p] . [up])
	    ([?\C-n] . [down])
	    ([?\C-a] . [home])
	    ([?\C-e] . [end])
	    ([?\M-v] . [prior])
	    ([?\C-v] . [next])
	    ([?\C-d] . [delete])
	    ([?\C-k] . [S-end delete])))
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
    (setq exwm-layout-show-all-buffers t)
    (setq exwm-workspace-show-all-buffers t)
    (exwm-input-set-key
     (kbd "<XF86MonBrightnessUp>")
     (lambda ()
       (interactive)
       (start-process-shell-command
	"light" nil "light -A 10")))
    (exwm-input-set-key
     (kbd "<XF86MonBrightnessDown>")
     (lambda ()
       (interactive)
       (start-process-shell-command
	"light" nil "light -U 10")))
    (setq exwm-input-global-keys
	  `(
	    ([?\s-r] . exwm-reset)
	    ([?\s-k]
	     . delete-window)
	    ([s-left] . windmove-left)
	    ([s-right] . windmove-right)
	    ([s-up] . windmove-up)
	    ([s-down] . windmove-down)
	    ([?\s-m] . exwm-workspace-move-window)
	    ([?\s-\ ] .
	     (lambda ()
	       (interactive)
	       (mg-starter)))
	    ([?\s-w] . exwm-workspace-switch)
	    ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
	    ([?\s-b] .
	     (lambda ()
	       (interactive)
	       (start-process "" nil mg-browser)))
	    ([?\s-i] .
	     (lambda ()
	       (interactive)
	       (start-process "" nil mg-keyboard-layout-changer "it")))
	    ([?\s-u] .
	     (lambda ()
	       (interactive)
	       (start-process "" nil mg-keyboard-layout-changer "us")))
	    ([?\s-f] .
	     (lambda ()
	       (interactive)
	       (mg-check-and-toggle-deepwork-mode)))))
    (setq exwm-workspace-index-map 
	  (lambda (index) (number-to-string (1+ index))))
    (add-hook 'exwm-init-hook
	      (lambda ()
		(progn
		  (start-process "dbus-update-activation-environment" nil "dbus-update-activation-environment" "DISPLAY")
		  (when (not (equal (system-name) mg-work-laptop-hostname))
		    (start-process "x-on-resize" nil "x-on-resize" "-c /home/claudio/Repositories/knock-files/cli-utils/monitor_hotplug.sh"))) t)))

  (use-package desktop-environment
    :straight t
    :after (exwm)
    :config
    (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment)
    (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement)
    (exwm-input-set-key (kbd "<XF86AudioMute>") #'desktop-environment-toggle-mute)
    (exwm-input-set-key (kbd "s-l") #'desktop-environment-lock-screen)
    (exwm-input-set-key (kbd "<XF86AudioPlay>") #'desktop-environment-toggle-music)
    (exwm-input-set-key (kbd "<XF86AudioPause>") #'desktop-environment-toggle-music)
    (exwm-input-set-key (kbd "<XF86AudioNext>") #'desktop-environment-music-next)
    (exwm-input-set-key (kbd "s-s") #'desktop-environment-screenshot-part)
    :custom
    (desktop-environment-volume-get-command "pamixer --get-volume")
    (desktop-environment-volume-set-command "pamixer %s")
    (desktop-environment-volume-toggle-regexp nil)
    (desktop-environment-volume-get-regexp "\\([0-9]+\\)")
    (desktop-environment-volume-normal-increment "-i 5 --allow-boost")
    (desktop-environment-volume-normal-decrement "-d 5")
    (desktop-environment-volume-toggle-command "pamixer -t")
    (desktop-environment-screenlock-command "xsecurelock"))

  (use-package time
    :straight t
    :after (exwm)
    :custom
    (display-time-format "[%d/%b %H:%M]")
    :config
    (display-time-mode)
    (display-battery-mode))

  (use-package mg-exwm
    :ensure nil
    :bind (("C-c u w l z" . mg-exwm-trigger-zurich-layout)
	   ("C-c u w l d" . mg-exwm-trigger-default-layout)
	   ("C-c u w l w" . mg-exwm-trigger-workstation-layout))))

(provide 'mg-emacs-exwm)
