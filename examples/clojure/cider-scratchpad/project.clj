(defproject scratchpad "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot scratchpad.core
  :global-vars  {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure                    "1.11.1"]
                 [org.clojure/tools.logging              "1.3.0"]
                 [org.clojure/tools.nrepl                "0.2.13"]
                 [cider/cider-nrepl                      "0.47.1"]
                 [ch.qos.logback/logback-classic         "1.5.3"]
                 [ch.qos.logback/logback-core            "1.5.3"]
                 [org.clojure/data.json                  "2.5.0"]
                 [prismatic/schema                       "1.4.1"]
                 [org.clojure/core.cache                 "1.1.234"]
                 [prismatic/schema-generators            "0.1.5"]
                 [com.rpl/specter                        "1.1.4"]

   [org.clojure/math.combinatorics         "0.2.0"]

   [com.cognitect.aws/api            "0.8.686"]
   [com.cognitect.aws/endpoints      "1.1.12.504"]
   [com.cognitect.aws/sqs            "847.2.1398.0"]
   [com.cognitect.aws/rds            "848.2.1413.0"]
   [com.cognitect.aws/sns            "847.2.1365.0"]
   [com.cognitect.aws/autoscaling    "847.2.1365.0"]
   [com.cognitect.aws/ecr            "847.2.1365.0"]
   [com.cognitect.aws/iam            "848.2.1413.0"]
   [com.cognitect.aws/route53domains "848.2.1400.0"]
   [com.cognitect.aws/redshift       "848.2.1413.0"]
   [com.cognitect.aws/route53        "847.2.1365.0"]
   [com.cognitect.aws/ec2            "848.2.1413.0"]
   [com.cognitect.aws/s3             "848.2.1413.0"]
   [com.cognitect.aws/cloudfront     "847.2.1365.0"]

   [clj-time                               "0.15.2"]
   [com.github.kyleburton/clj-etl-utils    "1.0.98"]
   [http-kit                               "2.7.0"]

   ;; https://mvnrepository.com/artifact/org.jsoup/jsoup
   [org.jsoup/jsoup                        "1.10.2"]])
