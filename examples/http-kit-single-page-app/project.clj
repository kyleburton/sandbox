(defproject single-page-app "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot single-page-app.core
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.3.0"]
                 [http-kit "2.1.16"]
                 [compojure "1.1.8"]
                 [ring "1.3.0"]
                 [org.clojure/data.json "0.2.5"]
                 [swank-clojure "1.4.3"]
                 [metrics-clojure "2.1.0"]
                 [metrics-clojure-ring "2.1.0"]
                 ;;[com.codahale.metrics/metrics-core "3.0.1"]
                 ])
