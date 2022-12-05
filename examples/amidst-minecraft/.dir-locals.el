;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((clojure-mode
  (krb-clj-cider-connect-args :host "localhost" :port "4046")
  (krb-clj-cider-connect-fn . cider-connect)
  (ffip-project-root       . "~/code/snapclean.me/amidst-minecraft")
  (krb-ag-search-directory . "~/code/snapclean.me/amidst-minecraft")
  (eval . (progn
            (require 'find-file-in-project)
            (add-to-list 'ffip-prune-patterns "*/vendor/bootstrap")
            (add-to-list 'ffip-prune-patterns "*/.shadow-cljs")
            (add-to-list 'ffip-prune-patterns "*/node_modules")
            (add-to-list 'ffip-prune-patterns "*resources/public/js/compiled")
            (add-to-list 'ffip-prune-patterns "*resources/public/vendor")
            (add-to-list 'ffip-prune-patterns "*assets")
            (add-to-list 'ffip-prune-patterns "*.clj-kondo")
            (add-to-list 'ffip-prune-patterns "*resources/public/admin/js/compiled")
            (add-to-list 'ffip-prune-patterns "*target/browser-test/js/cljs-runtime")))))
