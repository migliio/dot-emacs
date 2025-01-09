(use-package emacs
  :bind (("M-n" . forward-paragraph)
	 ("M-p" . backward-paragraph)
	 ("C-c u r s" . replace-string)
	 ("C-c u r q" . query-replace-regexp)
	 ("C-c u r r" . query-replace)
	 ("C-c u r c" . comment-region)
	 ("C-c u r u" . uncomment-region)
	 ("C-c u r i" . indent-region)
	 ("C-c u r a r" . align-regexp)
	 ("C-c u r a e" . align-entire)
	 ("C-c u c w" . whitespace-mode)
	 ("C-c u m" . compile)
	 ("C-c u x" . async-shell-command)
	 ("C-c u w f" . toggle-frame-fullscreen)
	 ("C-c u w m" . toggle-frame-maximized)
	 ("C-x C-n" . next-buffer)
	 ("C-x C-p" . previous-buffer))
  :init
  (global-set-key (kbd "C-x C-n") nil)
  (global-set-key (kbd "C-x C-p") nil)
  (defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
  (defconst mg-dot-private-file "~/Repositories/knock-files-private")
  (defconst mg-emacs-root "~/.emacs.d")
  (defconst mg-sendmail-bin "/usr/bin/msmtp")
  (setq inhibit-startup-screen t
	completion-cycle-threshold 3
	tab-always-indent 'complete
	create-lockfiles nil
	user-emacs-directory (expand-file-name "~/.cache/emacs/")
	bookmarks-file (expand-file-name ".bookmarks/bookmarks" mg-emacs-root)
	url-history-file (expand-file-name "url/history" user-emacs-directory)
	custom-file (if (boundp 'server-socket-dir)
			(expand-file-name "custom.el" server-socket-dir)
		      (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory))
	backup-by-copying t
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	message-send-mail-function 'message-send-mail-with-sendmail
	sendmail-program mg-sendmail-bin
	version-control t
	auto-save-list-file-prefix emacs-tmp-dir
	auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
	backup-directory-alist `((".*" . ,emacs-tmp-dir)))
  (setq-default frame-title-format '("%b")
		ring-bell-function 'ignore
		tab-width 8
		frame-resize-pixelwise t
		linum-format "%4d "
		use-short-answers t
		electric-indent-mode nil
		make-backup-files nil
		global-auto-revert-mode t
		confirm-kill-processes nil
		process-connection-type nil
		org-src-fontify-natively t
		;; warning-minimum-level :emergency
		set-language-environment "UTF-8"
		system-time-locale "C"
		native-comp-async-report-warnings-errors nil)
  (load custom-file t)
  (add-hook 'window-setup-hook 'toggle-frame-maximized t)
  (add-to-list 'yank-excluded-properties 'face)
  (if (display-graphic-p)
      (dolist (mode
	       '(tool-bar-mode
		 scroll-bar-mode
		 menu-bar-mode
		 tooltip-mode
		 blink-cursor-mode))
	(funcall mode 0)))
  (dolist (mode
	   '(global-visual-line-mode
	     show-paren-mode))
    (funcall mode 1)))

(use-package mg-macos
  :if (eq system-type 'darwin)
  :ensure nil
  :config
  (mg-macos-support-enable))

(use-package mg-emacs
  :after (mg-utils)
  :bind (("C-c p s" . mg-take-screenshot)
	 ("C-c u f" . mg-add-current-file-name-to-killring)
	 ("C-c u t" . mg-insert-today-timestamp-formatted)))

(use-package mg-defaults-extensions
  :ensure nil
  :config
  (add-hook 'prog-mode-hook
	    #'mg-line-numbers-highlight-line-mode))

(use-package mg-utils
  :ensure nil)

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package mg-modeline
  :ensure nil)

(provide 'mg-emacs-defaults)
