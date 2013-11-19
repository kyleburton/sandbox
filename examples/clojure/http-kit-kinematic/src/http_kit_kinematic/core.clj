(ns http-kit-kinematic.core
  (:require
   http-kit-kinematic.web.index
   [org.httpkit.server :as server]
   [kinematic.core     :as kcore]
   [kinematic.dsl      :as kdsl]))


(kdsl/defweb :test1
  :mount-point "/test1"
  :app-ns-prefix :http-kit-kinematic
  :before-middleware []
  :after-middleware []
  :404-page "resources/public/404.html")

(def stop-server-fn (atom nil))

(defn service-main []
  (reset! stop-server-fn (server/run-server (kdsl/dyn-handler :test1)
                                            {:port 8888})))

(comment

  (service-main)

)