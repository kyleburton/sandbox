(defproject xchart-examples "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot xchart-examples.core
  :global-vars  {*warn-on-reflection* true}
  :dependencies [
                 [org.clojure/clojure                    "1.9.0-beta4"]
                 [org.clojure/spec.alpha                 "0.1.143"]
                 [org.clojure/tools.logging              "1.2.1"]
                 [org.clojure/tools.nrepl                "0.2.12"]
                 [cider/cider-nrepl                      "0.16.0-SNAPSHOT"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [org.clojure/data.json                  "0.2.6"]
                 [clj-time                               "0.14.0"]
                 [org.clojure/core.cache                 "0.6.5"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.97"]
                 [http-kit                               "2.2.0"]
                 [camel-snake-kebab                      "0.4.0"]
                 [com.rpl/specter                        "1.0.0"]

                 [com.hypirion/clj-xchart "0.2.0"]
                 ])
