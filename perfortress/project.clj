(defproject com.github.kyleburton/perfortress "0.1.0"
  :description "Perforce P4Java Wrapper"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {
    :dev {:dependencies [
            [org.clojure/tools.nrepl                "0.2.11"]
            [cider/cider-nrepl                      "0.10.2"]
          ]}
  }
  :main ^:skip-aot perfortress.repl
  :dependencies [[org.clojure/clojure                    "1.8.0"]
                 [com.perforce/p4java                    "2015.2.1312871"]
                 [org.clojure/tools.logging              "0.3.1"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.0.5"]
                 [clj-time                               "0.11.0"]
                 [org.clojure/core.cache                 "0.6.4"]
                 [com.cemerick/url                       "0.1.1"]
                 [com.perforce/p4java                    "2015.2.1312871"]
  ])
