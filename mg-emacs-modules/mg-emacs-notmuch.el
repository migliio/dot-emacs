(use-package notmuch
  :straight t
  :bind (
	 :map global-map
	 ("C-x m" . notmuch)
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
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator "")
  (notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (notmuch-show-all-tags-list t)
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20s  ")
     ("subject" . "%-80s  ")
     ("tags" . "(%s)")))
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-80s  ")
     ("tags" . "(%s)")))
  (notmuch-show-empty-saved-searches t)
  (notmuch-saved-searches
   `(( :name "inbox"
       :query "tag:inbox"
       :sort-order newest-first
       :key ,(kbd "i"))
     ( :name "unread (inbox)"
       :query "tag:unread and tag:inbox"
       :sort-order newest-first
       :key ,(kbd "u"))
     ))
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
  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-citation-lines-prefix count
	  notmuch-wash-citation-lines-suffix count)))

(use-package ol-notmuch
  :straight (ol-notmuch :type git :host github :repo "tarsius/ol-notmuch")
  :after notmuch)

(provide 'mg-emacs-notmuch)
