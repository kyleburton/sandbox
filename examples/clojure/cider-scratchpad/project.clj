(defproject scratchpad "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot scratchpad.core
  :global-vars  {*warn-on-reflection* true}
  :dependencies
  [[org.clojure/clojure                    "1.10.0"]
   [org.clojure/tools.logging              "0.4.1"]
   ;; [org.clojure/tools.nrepl                "0.2.13"]
   [nrepl                                  "0.5.3"]
   [cider/cider-nrepl                      "0.20.0"]

   [ch.qos.logback/logback-classic         "1.2.3"]
   [org.clojure/data.json                  "0.2.6"]

   ;; TODO: convert yourself to clojure spec Kyle
   [prismatic/schema                       "1.1.10"]
   [clj-time                               "0.15.0"]
   [org.clojure/core.cache                 "0.7.2"]
   [com.github.kyleburton/clj-etl-utils    "1.0.98"]
   ;; [prismatic/schema-generators            "0.1.1"]
   [http-kit                               "2.3.0"]
   ;; [com.rpl/specter                        "1.0.5"]

   ;; [org.threeten/threetenbp                "1.3.3"]
   ;; https://mvnrepository.com/artifact/org.jsoup/jsoup
   [org.jsoup/jsoup                        "1.10.2"]])

