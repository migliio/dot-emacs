(use-package multiple-cursors
  :after (org)
  :straight t
  :bind (("C-c m >" . #'mc/mark-next-like-this)
	 ("C-c m <" . #'mc/mark-previous-like-this)
	 ("C-c m -" . #'mc/mark-next-like-this-word)
	 ("C-c m e" . 'mc/mark-more-like-this-extended)
	 ("C-c m s" . 'mc/mark-all-dwim)
	 ("C-c m a" . mc/mark-all-like-this)
	 ("C-c m r" . mc/mark-all-in-region)
	 ("C-c m d" . mc/mark-all-like-this-dwim)
	 ("C-c m w" . mc/mark-all-words-like-this))
  :custom
  (mc/always-run-for-all t)
  :init
  (require 'multiple-cursors)
  (define-key mc/keymap (kbd "<return>") nil)
  :config
  (multiple-cursors-mode 1))

(use-package deadgrep
  :straight t
  :bind
  (("M-g r" . deadgrep)))

(use-package xcscope
  :straight t
  :bind
  (("C-c s s" . cscope-find-this-symbol)
   ("C-c s d" . cscope-find-global-definition)
   ("C-c s c" . cscope-find-functions-calling-this-function)
   ("C-c s x" . cscope-set-initial-directory)
   ("C-c s f" . cscope-find-this-file))
  :config
  (cscope-setup))

(use-package ediff
  :straight t
  :config
  (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package yasnippet
  :straight t
  :init
  (defconst mg-snippets-dir ".snippets")
  :custom
  (yas-snippet-dirs (list (format "%s/%s" mg-emacs-root mg-snippets-dir)))
  :config
  (yas-global-mode 1))

(use-package mg-kernel
  :ensure nil
  :bind
  (:map c-mode-map
	("C-c v" . mg-get-kernel-version-from-source)
	("C-c ." . mg-kernel-do-grep)))

(when (display-graphic-p)
  (progn
    (use-package vterm
      :straight t)
    (use-package multi-vterm
      :straight t
      :bind (("C-c v" . multi-vterm)))))

(provide 'mg-emacs-programming)
