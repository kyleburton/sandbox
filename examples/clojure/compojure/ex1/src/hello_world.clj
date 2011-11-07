(ns hello-world
  (:use compojure.core, ring.adapter.jetty)
  (:require [compojure.route :as route]))

(defroutes main-routes
  (GET "/" [] "<h1>Hello World</h1>")
  (route/not-found "<h1>Page not found</h1>"))

(def *server* (run-jetty main-routes {:port 8080}))
