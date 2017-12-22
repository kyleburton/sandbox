(defproject the-mystery-on-orville-st "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot the-mystery-on-orville-st.core
  :dependencies [[org.clojure/clojure                    "1.9.0-RC1"]
                 [org.clojure/tools.logging              "0.4.0"]
                 [org.clojure/tools.nrepl                "0.2.12"]
                 [cider/cider-nrepl                      "0.15.1"]
                 [ch.qos.logback/logback-classic         "1.2.3"]
                 [metosin/muuntaja                       "0.4.1"]
                 [org.clojure/data.json                  "0.2.6"]
                 [prismatic/schema                       "1.1.7"]
                 [clj-time                               "0.14.2"]
                 [org.clojure/core.cache                 "0.6.5"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.98"]
                 [prismatic/schema-generators            "0.1.1"]

                 [http-kit                               "2.2.0"]
                 [compojure                              "1.6.0"]
                 [cprop                                  "0.1.11"]
                 [funcool/struct                         "1.1.0"]
                 [luminus-http-kit                       "0.1.5"]
                 [luminus-nrepl                          "0.1.4"]
                 [luminus/ring-ttl-session               "0.3.2"]
                 [com.cognitect/transit-clj              "0.8.300"]
                 [metosin/ring-http-response             "0.9.0"]
                 [mount                                  "0.1.11"]
                 [org.clojure/clojurescript              "1.9.946" :scope "provided"]
                 [org.clojure/tools.cli                  "0.3.5"]
                 [org.clojure/tools.reader               "1.1.0"]
                 [org.webjars.bower/tether               "1.4.0"]
                 [org.webjars/bootstrap                  "4.0.0-alpha.5"]
                 [org.webjars/font-awesome               "4.7.0"]
                 [ring-webjars                           "0.2.0"]
                 [ring/ring-core                         "1.6.3"]
                 [ring/ring-defaults                     "0.3.1"]
                 [selmer                                 "1.11.3"]
                 [markdown-clj                           "1.0.1"]

                 [com.rpl/specter                        "1.0.5"]])
