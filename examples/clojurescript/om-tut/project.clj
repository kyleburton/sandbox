(defproject om-tut "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

 :profiles 
  {:dev 
    {:plugins [[com.cemerick/austin "0.1.4"]]}}


  :dependencies [[org.clojure/clojure        "1.6.0"]
                 ;; austin says it's not compatible with the latest
                 ;; clojurescript, but is compatible with this version, though
                 ;; it won't build...
                 ;; [org.clojure/clojurescript  "0.0-2156"]
                 [org.clojure/clojurescript "0.0-2268"]
                 [org.clojure/core.async     "0.1.267.0-0d7780-alpha"]
                 [om                         "0.6.5"]]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild { 
    :builds [{:id "om-tut"
              :source-paths ["src"]
              :compiler {
                :output-to "om_tut.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
