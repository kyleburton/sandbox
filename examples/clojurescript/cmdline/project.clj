(defproject cmdline "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure       "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]
                 [org.clojure/core.async    "0.1.346.0-17112a-alpha"]
                 [prismatic/schema          "0.3.1"]]

  :plugins [
            [lein-cljsbuild         "1.0.4-SNAPSHOT"]
            ]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "cmdline"
              :source-paths ["src"]
              :compiler {
                :output-to "out/cmdline.js"
                :output-dir "out"
                :target :nodejs
                :optimizations :none
                :source-map true}}]})
