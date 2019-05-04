(defproject scratchpad "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot scratchpad.core
  :global-vars  {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure                   "1.10.0"]
                 [org.clojure/tools.logging              "0.4.1"]
                 [org.clojure/tools.nrepl                "0.2.13"]
                 [cider/cider-nrepl                      "0.21.1"]
                 [ch.qos.logback/logback-classic         "1.2.3"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.1.7"]
                 [clj-time                               "0.14.2"]
                 [org.clojure/core.cache                 "0.6.5"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.98"]
                 [prismatic/schema-generators            "0.1.1"]
                 [http-kit                               "2.2.0"]
                 [com.rpl/specter                        "1.0.5"]

   [ch.qos.logback/logback-classic         "1.2.3"]
   [org.clojure/data.json                  "0.2.6"]

   [com.cognitect.aws/api            "0.8.243"]
   [com.cognitect.aws/endpoints      "1.1.11.490"]
   [com.cognitect.aws/autoscaling    "697.2.391.0"]
   [com.cognitect.aws/cloudfront     "697.2.391.0"]
   [com.cognitect.aws/ec2            "698.2.395.0"]
   [com.cognitect.aws/ecr            "701.2.394.0"]
   [com.cognitect.aws/iam            "697.2.391.0"]
   [com.cognitect.aws/rds            "701.2.394.0"]
   [com.cognitect.aws/redshift       "697.2.391.0"]
   [com.cognitect.aws/route53        "697.2.391.0"]
   [com.cognitect.aws/route53domains "697.2.391.0"]
   [com.cognitect.aws/s3             "697.2.391.0"]
   [com.cognitect.aws/sns            "697.2.391.0"]
   [com.cognitect.aws/sqs            "697.2.391.0"]

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

