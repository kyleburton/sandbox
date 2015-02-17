(defproject graph-services "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot graph-services.core
  :dependencies [
                 [org.clojure/tools.logging              "0.3.0"]
                 [http-kit                               "2.1.16"]
                 [org.clojure/clojure                    "1.6.0"]
                 [org.clojure/tools.nrepl                "0.2.3"]
                 [cider/cider-nrepl                      "0.7.0"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [com.github.kyleburton/clj-lfsr         "1.3.1"]
                 [org.clojure/data.json                  "0.2.5"]
                 [prismatic/schema                       "0.3.1"]
                 [clj-time                               "0.8.0"]
                 [ring                                   "1.3.0"]
                 [ring/ring-jetty-adapter                "1.3.0"]
                 [com.relaynetwork/clorine               "1.0.18"]
                 [org.clojure/data.csv                   "0.1.2"]
                 [kinematic-middleware                   "0.1.0"]
                 [org.clojure/math.combinatorics         "0.0.8"]
                 [cheshire                               "5.3.1"]
                 [clojurewerkz/neocons                   "3.0.0"]
                 ])
