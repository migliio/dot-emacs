(use-package ellama
  :straight t
  :custom
  (ellama-keymap-prefix "C-c u a")
  (ellama-language "English")
  :config
  (require 'llm-ollama)
  (cond
   ((eq system-type 'darwin)
    (setq ellama-provider
	  (make-llm-ollama
	   :chat-model "llama3.3:70b")))
   ((equal (system-name) "think")
    (setq ellama-provider
	  (make-llm-ollama
	   :chat-model "phi4:latest")))
   (t
    (setq ellama-provider
	  (make-llm-ollama
	   :chat-model "llama3.3:latest")))))

(provide 'mg-emacs-ai)
