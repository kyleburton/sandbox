(defproject mongo-start "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot mongo-start.core
  :global-vars  {*warn-on-reflection* true}
  :dependencies [
                 [org.clojure/clojure                    "1.9.0-beta4"]
                 [org.clojure/spec.alpha                 "0.1.143"]
                 [org.clojure/tools.logging              "0.4.0"]
                 [org.clojure/tools.nrepl                "0.2.12"]
                 ;; [cider/cider-nrepl                      "0.15.1"]
                 [cider/cider-nrepl                      "0.16.0-SNAPSHOT"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.1.7"]
                 [clj-time                               "0.14.0"]
                 [org.clojure/core.cache                 "0.6.5"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.97"]
                 [http-kit                               "2.2.0"]
                 [camel-snake-kebab                      "0.4.0"]
                 [com.rpl/specter                        "1.0.0"]

                 ;; NB: this is the java driver
                 [org.mongodb/mongodb-driver             "3.5.0"]
                 ;; Clj library, last change in May 2017
                 ;; http://clojuremongodb.info/
                 [com.novemberain/monger                 "3.1.0"]

                 ;; can't open our file :(
                 ;; [de.slackspace/openkeepass              "0.6.1"]

                 ;; can't open our file :(
                 ;; [org.linguafranca.pwdb/KeePassJava2     "2.1.1"]


                 ])
