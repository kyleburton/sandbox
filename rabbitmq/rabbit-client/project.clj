(defproject 
  rabbit-client "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :dev-resources-path "dev-resources"
  :java-source-path [["java"]]
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [log4j/log4j "1.2.14"]
                 [swank-clojure/swank-clojure "1.2.1"]
                 ;;[com.rabbitmq/amqp-client "1.7.2"]
                 [com.rabbitmq/amqp-client "2.5.0"]
                 [org.clojars.kyleburton/clj-etl-utils "1.0.34"]
                 [com.relaynetwork/clorine "1.0.4"]])

