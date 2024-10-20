(use-package ellama
  :straight t
  :custom
  (ellama-keymap-prefix "C-c u a")
  (ellama-language "English")
  :config
  (require 'llm-ollama)
  (setq ellama-provider
	(make-llm-ollama
	 :chat-model "llama3.2:latest")))

(provide 'mg-emacs-ai)
