(defproject web-service "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot web-service.core
  :jvm-opts ["-Xmx256m" "-server"]
  :dependencies [[org.clojure/tools.logging              "0.3.0"]
                 [http-kit                               "2.1.16"]
                 [org.clojure/clojure                    "1.6.0"]
                 [org.clojure/tools.nrepl                "0.2.3"]
                 [cider/cider-nrepl                      "0.7.0"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [clojurewerkz/cassaforte                "2.0.0-beta1"]
                 [com.github.kyleburton/clj-lfsr         "1.3.1"]
                 [org.clojure/data.json                  "0.2.5"]
                 [prismatic/schema                       "0.3.1"]
                 [clj-time                               "0.8.0"]
                 [ring                                   "1.3.0"]
                 [ring/ring-jetty-adapter                "1.3.0"]
                 [org.clojure/data.fressian              "0.2.0"]
                 [org.mindrot/jbcrypt                    "0.3m"]
                 [jarohen/chord                          "0.4.2"]
                 [com.draines/postal                     "1.11.1"]
                 [org.clojure/core.cache                 "0.6.4"]
                 [com.relaynetwork/clorine               "1.0.18"]
                 [org.clojure/data.csv                   "0.1.2"]
                 [clojurewerkz/quartzite                 "1.3.0"]
                 [clj-pdf                                "1.11.21"]
                 [amazonica                              "0.3.1"
                   :exclusions [joda-time]]
                 [kinematic-middleware                   "0.1.0"]
                 [org.clojure/core.typed                 "0.2.72"]])
