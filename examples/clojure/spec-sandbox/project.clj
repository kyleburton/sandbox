(defproject spec-sandbox "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot spec-sandbox.repl
  :dependencies [
    [org.clojure/clojure "1.9.0-alpha12"]
    [org.clojure/tools.logging              "0.3.1"]
    [org.clojure/tools.nrepl                "0.2.12"]
    [cider/cider-nrepl                      "0.13.0"]
    [ch.qos.logback/logback-classic         "1.0.13"]
    [org.clojure/data.json                  "0.2.6"]
    [clj-time                               "0.12.0"]
    ])
