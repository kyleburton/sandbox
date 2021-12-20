(add-to-list 'slime-lisp-implementations
             '(kburton-clojure ("@bin.dir@/repl") 
                               :init swank-clojure-init
                               :init-function krb-swank-clojure-init) t)
