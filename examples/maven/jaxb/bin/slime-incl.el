(add-to-list 'slime-lisp-implementations
             '(jaxb-example ("/Users/kburton/personal/projects/sandbox/examples/maven/jaxb/bin/repl")
                                    :init swank-clojure-init
                                    :init-function krb-swank-clojure-init))
