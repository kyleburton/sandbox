;; local emacs configuration for Land of Lisp exercises

;; configure SLIME for clisp to follow along with the book
(require 'slime-autoloads)

(setq slime-lisp-implementations
      '((local-clisp ("clisp" "-i" ".clisp.init"))))

(setq inferior-lisp-program "./run-clisp")
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)


