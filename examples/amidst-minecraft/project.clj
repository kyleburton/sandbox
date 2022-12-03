(defproject amidst-minecraft "0.1.0-SNAPSHOT"
  :description   "FIXME: write description"
  :url           "http://example.com/FIXME"
  :license       {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
                  :url "https://www.eclipse.org/legal/epl-2.0/"}
  :repl-options  {:init-ns amidst-minecraft.core}
  :aot           [amidst-minecraft.core]
  :main          amidst-minecraft.core
  :profiles     {:dev {:dependencies [[org.typedclojure/typed.clj.checker "1.0.31"]]}}
  :plugins      [[lein-ancient "1.0.0-RC3"]
                 [lein-typed "0.4.6"]]
  :core.typed   {:check [amidst-minecraft.core]}
  :dependencies  [[org.clojure/clojure                    "1.11.1"]
                  [org.typedclojure/typed.clj.runtime     "1.0.31"]
                  [org.clojure/tools.logging              "1.2.4"]
                  [nrepl/nrepl                            "0.9.0"]
                  [cider/cider-nrepl                      "0.28.5"]
                  [ch.qos.logback/logback-classic         "1.2.11"]
                  [org.clojure/data.json                  "2.4.0"]])
