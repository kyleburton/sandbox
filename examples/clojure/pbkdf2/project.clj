(defproject pbkdf2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot pbkdf2.core
  :global-vars  {*warn-on-reflection* true}
  :dependencies [;; [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojure                    "1.9.0-alpha3"]
                 [org.clojure/tools.logging              "1.2.1"]
                 [org.clojure/tools.nrepl                "0.2.12"]
                 [cider/cider-nrepl                      "0.13.0"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.1.3"]
                 [clj-time                               "0.12.0"]
                 [org.clojure/core.cache                 "0.6.4"]])
