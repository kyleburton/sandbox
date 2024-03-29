(defproject cider-nrepl-scratchpad "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot cider-nrepl-scratchpad.core
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging              "1.2.1"]
                 [org.clojure/tools.nrepl                "0.2.11"]
                 [cider/cider-nrepl                      "0.10.2"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.0.5"]
                 [clj-time                               "0.11.0"]
                 [org.clojure/core.cache                 "0.6.4"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.94"]

                 ;; web backend development http://www.http-kit.org/
                 [http-kit                               "2.1.18"]
                 [compojure                              "1.4.0"]
                 [ring                                   "1.4.0"]

                 ;; TODO: aws library / examples: sqs, sns
                 ;; TODO: amqp show pub/sub w/rabbit using http://clojurerabbitmq.info/
                 ;; TODO: kafka
                 ;; TODO: an example pedestal app
                 ;; TODO: storm
                 ;; TODO: spark
                 ])
