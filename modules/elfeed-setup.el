(use-package elfeed
  :ensure t
  :bind (("C-c e" . elfeed))
  :config
  ;; Somewhere in your .emacs file
  (setq elfeed-feeds
	'("https://news.ycombinator.com/rss"
	  "https://seclists.org/rss/fulldisclosure.rss"
	  "https://xkcd.com/atom.xml"
	  "https://feeds.feedburner.com/TheHackersNews?format=xml"
	  "https://www.kernel.org/feeds/kdist.xml"
	  "https://9to5linux.com/feed/atom"
	  "https://www.phoronix.com/rss.php"
	  "https://www.freelists.org/feed/cryptome"
	  "https://www.schneier.com/feed/")))

(provide 'elfeed-setup)
