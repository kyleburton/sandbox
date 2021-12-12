(defproject gherkin "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot gherkin.core
  :jvm-opts ["-Xmx512m" "-server" "-Dorg.quartz.scheduler.skipUpdateCheck=true"]
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/tools.logging              "1.2.1"]
                 [org.clojure/tools.nrepl                "0.2.3"]
                 [cider/cider-nrepl                      "0.7.0"]
                 [org.clojure/data.json                  "0.2.5"]
                 [prismatic/schema                       "0.3.1"]
                 [clj-time                               "0.8.0"]
                 [org.clojure/core.cache                 "0.6.4"]
                 [org.clojure/data.csv                   "0.1.2"]
                 ])
