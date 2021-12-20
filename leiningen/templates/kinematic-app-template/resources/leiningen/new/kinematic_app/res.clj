(ns {{name}}.api.v1.res
  (:require
   [kinematic.http                                :refer [api-get]]
   [kinematic.routes                              :refer [route]]))

(route :api-v1 ["/res"])

;; NB: you must have one of these roles: root, organizer
(api-get
  {:status "OK"
   :message "Hello, Kinematic, this is /res"})
