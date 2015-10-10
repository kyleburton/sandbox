(with-temp-file "tmp/emacs-info.txt"
  (insert "emacs-version=" emacs-version "\n"))
(save-buffers-kill-terminal)
