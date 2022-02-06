((clojurescript-mode
  (krb-clj-cider-connect-args :host "localhost" :port "8779" :cljs-repl-type shadow)
  (cider-shadow-default-options . "app")
  (krb-clj-cider-connect-fn . cider-connect-cljs)
  (ag-ignore-list .
                  ("vendor/bootstrap"
                   "js/compiled"
                   "target/stale"))
  (ffip-project-root       . "~/code/snapclean.me/artzonewestla.com/artzone-web")
  (krb-ag-search-directory . "~/code/snapclean.me/artzonewestla.com/artzone-web")
  (eval . (progn
            (require 'find-file-in-project)
            ;; NB: the find command indicated that anything ending with a '/'
            ;; can't possibly match
            (add-to-list 'ffip-prune-patterns "*/vendor/bootstrap")
            (add-to-list 'ffip-prune-patterns "*/.shadow-cljs")
            (add-to-list 'ffip-prune-patterns "*/node_modules")
            (add-to-list 'ffip-prune-patterns "*resources/public/js/compiled")
            (add-to-list 'ffip-prune-patterns "*/resources/public/vendor")
            (add-to-list 'ffip-prune-patterns "*.clj-kondo")
            (add-to-list 'ffip-prune-patterns "*target/browser-test") ;; not working?
            (add-to-list 'ffip-prune-patterns "target/browser-test"))))
 (nil . ((cider-default-cljs-repl . shadow)
         (cider-shadow-cljs-default-options . "app"))))
