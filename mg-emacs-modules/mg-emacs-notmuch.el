(use-package notmuch
  :straight t
  :bind (
	 :map global-map
	 ("C-x m" . nil)
	 ("C-x m o" . notmuch)
	 :map notmuch-search-mode-map
	 ("a" . nil)
	 ("A" . nil)
	 ("/" . notmuch-search-filter)
	 ("r" . notmuch-search-reply-to-thread)
	 ("R" . notmuch-search-reply-to-thread-sender)
	 :map notmuch-show-mode-map
	 ("a" . nil)
	 ("A" . nil)
	 ("r" . notmuch-show-reply)
	 ("R" . notmuch-show-reply-sender))
  :custom
  (notmuch-show-logo nil)
  (notmuch-search-oldest-first nil)
  (notmuch-archive-tags nil
			notmuch-message-replied-tags '("+replied")
			notmuch-message-forwarded-tags '("+forwarded")
			notmuch-show-mark-read-tags '("-unread")
			notmuch-draft-tags '("+draft")
			notmuch-draft-folder "drafts"
			notmuch-draft-save-plaintext 'ask)
  (notmuch-show-relative-dates t)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-show-text/html-blocked-images ".") ; block everything
  (notmuch-wash-wrap-lines-length 120)
  (notmuch-unthreaded-show-out nil)
  (notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (notmuch-message-headers-visible t)
  :config
  (add-to-list 'notmuch-saved-searches '(:name "mm" :query "tag:mm" :key "m"))
  (defun pop-from-message-completion()
    (pop message--old-style-completion-functions))
  (advice-add 'message-completion-function :after #'pop-from-message-completion)
  (let ((count most-positive-fixnum))
    (setq notmuch-wash-citation-lines-prefix count
	  notmuch-wash-citation-lines-suffix count)))

(use-package ol-notmuch
  :straight (ol-notmuch :type git :host github :repo "tarsius/ol-notmuch")
  :after notmuch)

(use-package mg-notmuch
  :ensure nil
  :after notmuch
  :bind (("C-x m u" . mg-notmuch-update-mail))
  :config

  (setq notmuch-hello-refresh-hook #'mg-notmuch-update-mail))

(provide 'mg-emacs-notmuch)
;;; mg-emacs-notmuch.el ends here
