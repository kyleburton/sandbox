(defproject cljs-sandbox "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :plugins [
    [lein-cljsbuild "0.3.2"]
    [lein-ring "0.8.7"]
  ]
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [org.clojure/clojurescript "0.0-1806"]
    [compojure "1.1.5"]
    [jayq "2.4.0"]
    [hiccup "1.0.4"]
  ]
  :source-paths
  ["src/clj"]
  :cljsbuild {
    :builds {
      :main {
        :source-path "src/cljs"
        :compiler {
          :output-to "resources/public/js/cljs.js"
          :optimizations :simple
          :pretty-print true
        }
      }
    }
  }
  :main cljs-sandbox.core
  :ring {:handler cljs-sandbox.core}
)
