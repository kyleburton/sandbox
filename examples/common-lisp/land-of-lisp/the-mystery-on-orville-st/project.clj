(defproject the-mystery-on-orville-st "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot the-mystery-on-orville-st.core
  :dependencies [[org.clojure/clojure                    "1.9.0-RC1"]
                 [org.clojure/tools.logging              "0.4.0"]
                 [org.clojure/tools.nrepl                "0.2.12"]
                 [cider/cider-nrepl                      "0.15.1"]
                 [ch.qos.logback/logback-classic         "1.2.3"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.1.7"]
                 [clj-time                               "0.14.2"]
                 [org.clojure/core.cache                 "0.6.5"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.98"]
                 [prismatic/schema-generators            "0.1.1"]
                 [http-kit                               "2.2.0"]
                 [com.rpl/specter                        "1.0.5"]])
