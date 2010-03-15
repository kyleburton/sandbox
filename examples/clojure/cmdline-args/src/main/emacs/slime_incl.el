
(setq slime-lisp-implementations
      '((cmdline-args ("@bin.dir@/repl")
                     :init swank-clojure-init
                     :init-function krb-swank-clojure-init)))
