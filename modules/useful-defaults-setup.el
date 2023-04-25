(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq-default frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(global-visual-line-mode t)
(winner-mode t)
(setq-default tab-width 8)
(setq-default c-basic-offset 8)
(setq confirm-kill-processes nil)
(setq frame-resize-pixelwise t)

;; Some useful global keybinds
(global-set-key (kbd "C-c h") 'winner-undo)
(global-set-key (kbd "C-c l") 'winner-redo)
(global-set-key (kbd "C-c c k s") 'replace-string)
(global-set-key (kbd "C-c c k c") 'comment-region)
(global-set-key (kbd "C-c c k u") 'uncomment-region)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(setq linum-format "%4d ")
(global-auto-revert-mode t)
(unless (display-graphic-p)
  (add-hook 'prog-mode-hook
            #'display-line-numbers-mode
            #'linum-mode))
(setq display-line-numbers-type 'relative)

;; Avoid boring warnings
;; (setq warning-minimum-level :emergency)

;; Disable some boring modes
(dolist (mode
         '(tool-bar-mode
           scroll-bar-mode
	   menu-bar-mode
           blink-cursor-mode))
  (funcall mode 0))

;; Solves a bug with xdg-open
(setq process-connection-type nil)

;; System locale to use for formatting time values
(setq system-time-locale "C")

;; Use UTF-8 as a preferred coding system
(set-language-environment "UTF-8")

;; Avoid async shell commands warnings
(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(provide 'useful-defaults-setup)
