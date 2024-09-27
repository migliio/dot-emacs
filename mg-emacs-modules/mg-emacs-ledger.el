(use-package ledger-mode
  :straight t
  :init
  (defconst mg-ledger-bin "/usr/bin/hledger")
  :mode ("\\.journal\\'" "\\.ledger\\'" "\\.hledger\\'")
  :custom
  (ledger-binary-path mg-ledger-bin)
  (ledger-mode-should-check-version nil)
  (ledger-report-auto-width nil)
  (ledger-report-use-native-highlighting nil))

(provide 'mg-emacs-ledger)
