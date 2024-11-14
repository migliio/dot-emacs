(use-package anki-editor
  :straight (:type git :host github :repo "anki-editor/anki-editor" :branch "master")
  :bind
  (("C-c o a i" . anki-editor-insert-note)
   ("C-c o a p" . anki-editor-push-notes)))

(provide 'mg-emacs-flashcards)
