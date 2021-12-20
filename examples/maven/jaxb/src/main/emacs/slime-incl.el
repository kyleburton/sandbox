(add-to-list 'slime-lisp-implementations
             '(@project.artifactId@ ("@bin.dir@/repl")
                                    :init swank-clojure-init
                                    :init-function krb-swank-clojure-init))
