(ns kinematic-example.api.v1.users
  (:require
   [kinematic.dsl :refer [defapi api-get]]))

(defapi :example1 ["api/v1/users"])

(api-get
 {:status "OK"
  :message "Hello there"})
