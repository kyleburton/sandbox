(defproject scratchpad "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot scratchpad.core
  :global-vars  {*warn-on-reflection* true}
  :dependencies [
                 [org.clojure/clojure                    "1.8.0"]
                 [org.clojure/tools.logging              "1.2.1"]
                 [org.clojure/tools.nrepl                "0.2.12"]
                 [cider/cider-nrepl                      "0.10.2"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.0.5"]
                 [clj-time                               "0.11.0"]
                 [org.clojure/core.cache                 "0.6.4"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.88"]
                 [prismatic/schema-generators            "0.1.0"]

                 [com.google.guava/guava                 "19.0"]
                 [com.github.docker-java/docker-java     "3.0.0-RC4"]
                 [http-kit "2.1.18"]

                 [org.clojure/java.jdbc "0.6.0-rc1"]
                 [org.postgresql/postgresql "9.4-1206-jdbc42"]
                 [com.google.code.gson/gson "2.6.2"]

                 ;; [org.json/json "20160212"]
                 ;; [org.json/json "20160810"]

                 ])

