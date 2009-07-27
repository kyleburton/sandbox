(add-to-list 'slime-lisp-implementations
             '(krbclj ("@bin.dir@/repl") 
                        :init swank-clojure-init
                        :init-function krb-swank-clojure-init) t)