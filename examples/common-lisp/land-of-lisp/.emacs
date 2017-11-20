;; local emacs configuration for Land of Lisp exercises

;; configure SLIME for clisp to follow along with the book
(require 'slime-autoloads)

(setq slime-lisp-implementations
      '((local-clisp ("clisp" "-i" ".clisp.init"))))

(setq inferior-lisp-program "./run-clisp")
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)


(defun krb-slime-inspect-expr-before-point ()
  (interactive)
  (save-excursion
    (backward-sexp 1)
    (let ((start (point)))
      (forward-sexp 1)
      (message "(slime-inspect %s)" (buffer-substring-no-properties start (point)))
      (slime-inspect (buffer-substring-no-properties start (point))))))

(defun krb-slime-mode-hook ()
  (local-set-key (kbd "C-c M-i") 'krb-slime-inspect-expr-before-point))

(add-hook 'slime-mode-hook             #'krb-slime-mode-hook)
