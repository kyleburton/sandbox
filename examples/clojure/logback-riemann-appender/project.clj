(defproject logback-riemann-appender "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["java"]
  :plugins [[lein-release/lein-release "1.0.5"]
            [lein-swank                "1.4.5"]]
  :dependencies [
    [org.clojure/clojure            "1.5.1"]
    [com.aphyr/riemann-java-client  "0.2.4"]
    [ch.qos.logback/logback-classic "1.0.13"]
    [org.clojure/tools.logging      "0.2.6"]
  ])
