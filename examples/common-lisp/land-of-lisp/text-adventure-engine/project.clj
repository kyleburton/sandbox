(defproject text-adventure-engine "0.1.0-SNAPSHOT"

  :description "FIXME: write description"
  :url "http://example.com/FIXME"

  :dependencies [[ch.qos.logback/logback-classic "1.2.3"]
                 [clj-time                   "0.14.2"]
                 [compojure                  "1.6.0"]
                 [cprop                      "0.1.11"]
                 [funcool/struct             "1.1.0"]
                 [luminus-http-kit           "0.1.5"]
                 [luminus-nrepl              "0.1.4"]
                 [luminus/ring-ttl-session   "0.3.2"]
                 [markdown-clj               "1.0.1"]
                 [metosin/muuntaja           "0.4.1"]
                 [metosin/ring-http-response "0.9.0"]
                 [mount                      "0.1.11"]
                 [org.clojure/clojure        "1.8.0"]
                 [org.clojure/clojurescript  "1.9.946" :scope "provided"]
                 [org.clojure/tools.cli      "0.3.5"]
                 [org.clojure/tools.logging  "0.4.0"]
                 [org.clojure/tools.reader   "1.1.0"]
                 [org.webjars.bower/tether   "1.4.0"]
                 [org.webjars/bootstrap      "4.0.0-alpha.5"]
                 [org.webjars/font-awesome   "4.7.0"]
                 [ring-webjars               "0.2.0"]
                 [ring/ring-core             "1.6.3"]
                 [ring/ring-defaults         "0.3.1"]
                 [selmer                     "1.11.3"]

                 ;; [org.clojure/tools.nrepl                "0.2.12"]
                 ;; [cider/cider-nrepl                      "0.15.1"]
                 [cider/cider-nrepl                      "0.16.0-SNAPSHOT"]
                 [prismatic/schema                       "1.1.7"]
                 [org.clojure/core.cache                 "0.6.5"]
                 [com.github.kyleburton/clj-etl-utils    "1.0.98"]

                 ]

  :min-lein-version "2.0.0"

  :jvm-opts       ["-server" "-Dconf=.lein-env"]
  :source-paths   ["src/clj" "src/cljc"]
  :test-paths     ["test/clj"]
  :resource-paths ["resources" "target/cljsbuild"]
  :target-path    "target/%s/"
  :main ^:skip-aot text-adventure-engine.core

  :plugins [[lein-cprop "1.0.3"]
            [lein-cljsbuild "1.1.5"]]
  :clean-targets ^{:protect false}
  [:target-path [:cljsbuild :builds :app :compiler :output-dir] [:cljsbuild :builds :app :compiler :output-to]]
  :figwheel
  {:http-server-root "public"
   :nrepl-port 7002
   :css-dirs ["resources/public/css"]
   :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}


  :profiles
  {:uberjar {:omit-source true
             :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
             :cljsbuild
             {:builds
              {:min
               {:source-paths ["src/cljc" "src/cljs" "env/prod/cljs"]
                :compiler
                {:output-to "target/cljsbuild/public/js/app.js"
                 :optimizations :advanced
                 :pretty-print false
                 :closure-warnings
                 {:externs-validation :off :non-standard-jsdoc :off}}}}}


             :aot :all
             :uberjar-name "text-adventure-engine.jar"
             :source-paths ["env/prod/clj"]
             :resource-paths ["env/prod/resources"]}

   :dev           [:project/dev :profiles/dev]
   :test          [:project/dev :project/test :profiles/test]

   :project/dev  {:dependencies [[prone "1.1.4"]
                                 [ring/ring-mock "0.3.1"]
                                 [ring/ring-devel "1.6.3"]
                                 [pjstadig/humane-test-output "0.8.3"]
                                 [binaryage/devtools "0.9.7"]
                                 [com.cemerick/piggieback "0.2.2"]
                                 [doo "0.1.8"]
                                 [figwheel-sidecar "0.5.14"]]
                  :plugins      [[com.jakemccrary/lein-test-refresh "0.19.0"]
                                 [lein-doo "0.1.8"]
                                 [lein-figwheel "0.5.14"]
                                 [org.clojure/clojurescript "1.9.946"]]
                  :cljsbuild
                  {:builds
                   {:app
                    {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
                     :figwheel
                     {:on-jsload "text-adventure-engine.core/mount-components"}
                     :compiler
                     {:main "text-adventure-engine.app"
                      :asset-path "/js/out"
                      :output-to "target/cljsbuild/public/js/app.js"
                      :output-dir "target/cljsbuild/public/js/out"
                      :source-map true
                      :optimizations :none
                      :pretty-print true}}}}



                  :doo {:build "test"}
                  :source-paths ["env/dev/clj"]
                  :resource-paths ["env/dev/resources"]
                  :repl-options {:init-ns user}
                  :injections [(require 'pjstadig.humane-test-output)
                               (pjstadig.humane-test-output/activate!)]}
   :project/test {:resource-paths ["env/test/resources"]
                  :cljsbuild
                  {:builds
                   {:test
                    {:source-paths ["src/cljc" "src/cljs" "test/cljs"]
                     :compiler
                     {:output-to "target/test.js"
                      :main "text-adventure-engine.doo-runner"
                      :optimizations :whitespace
                      :pretty-print true}}}}

                  }
   :profiles/dev {}
   :profiles/test {}})
