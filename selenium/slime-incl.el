(add-to-list 'slime-lisp-implementations
             '(kburton-clojure ("/Users/kburton/personal/projects/sandbox/selenium/repl") 
                               :init swank-clojure-init
                               :init-function krb-swank-clojure-init) t)
