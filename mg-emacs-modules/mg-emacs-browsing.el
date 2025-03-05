(use-package engine-mode
  :straight t
  :config
  (engine/set-keymap-prefix (kbd "C-c u e"))
  (defun mg-engine-mode-exact-phrase-transform (search-term)
    (if current-prefix-arg
	(concat "\"" search-term "\"")
      search-term))
  (defengine archwiki
    "https://wiki.archlinux.org/index.php?search=%s"
    :keybinding "a")
  (defengine google
    "http://www.google.com/search?hl=en&ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"
    :term-transformation-hook mg-engine-mode-exact-phrase-transform)
  (defengine elixir
    "https://elixir.bootlin.com/linux/latest/A/ident/%s"
    :keybinding "k")
  (defengine kernel-documentation
    "https://www.kernel.org/doc/html/v4.12/core-api/kernel-api.html#c.%s"
    :keybinding "d")
  (defengine syscall-table
    "https://syscalls.mebeim.net/?table=x86/64/x64/latest"
    :keybinding "s")
  (defengine google-maps
    "https://www.google.com/maps/search/%s/"
    :keybinding "M")
  (defengine semantic-scholar
    "https://www.semanticscholar.org/search?q=%s&sort=relevance"
    :keybinding "r")
  (defengine openstreetmap
    "https://www.openstreetmap.org/search?query=%s"
    :keybinding "m")
  (defengine wordreference-iten
    "https://www.wordreference.com/iten/%s"
    :keybinding "i")
  (defengine wordreference-enit
    "https://www.wordreference.com/enit/%s"
    :keybinding "e")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  (engine-mode t))

(use-package eww
  :ensure nil
  :config
  (setq shr-use-fonts nil)
  (setq shr-color-visible-luminance-min 80)
  (setq shr-use-colors nil))

(provide 'mg-emacs-browsing)
