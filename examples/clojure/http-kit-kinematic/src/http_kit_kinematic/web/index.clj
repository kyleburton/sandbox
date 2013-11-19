(ns http-kit-kinematic.web.index
  (:require
   [kinematic.dsl      :as kdsl]))


(kdsl/defapi :test1 ["/index"])

(kdsl/api-get
 {:status "OK"
  :message "Hello Kinematic!"})