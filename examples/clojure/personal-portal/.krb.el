(defun krb-clojure-cider-jack-in ()
  (interactive)
  (cider-connect-clj '(:host "localhost" :port "4000")))
