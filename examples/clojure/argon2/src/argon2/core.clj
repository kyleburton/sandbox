(ns argon2.core
  (:require
   [clojure.tools.nrepl.server                 :refer [start-server stop-server]]
   [cider.nrepl                                :refer [cider-nrepl-handler]]
   [clojure.tools.logging                      :as log]
   [schema.core                                :as s]))

(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4002}}))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config)
  (s/set-fn-validation! true))



(comment


  ;; https://github.com/phxql/argon2-jvm

  (de.mkammerer.argon2/ArgonFactory)

  (let [argon2 (de.mkammerer.argon2.Argon2Factory/create)
        r      2
        n      65536
        p      1
        passwd (.toCharArray "foo")]
    (.hash argon2 r n p passwd))

)

