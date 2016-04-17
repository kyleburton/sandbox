(defproject repl-from-java "1.0.1"
  :description "CIDER + Nepl in a single dependency."
  :url "https://github.com/kyleburton/sandbox/tree/master/examples/clojure/repl-from-java"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :main ^:skip-aot com.github.kyleburton.Repl
  :dependencies [
                 [org.clojure/clojure                    "1.8.0"]
                 [org.clojure/tools.logging              "0.3.1"]
                 [org.clojure/tools.nrepl                "0.2.12"]
                 [cider/cider-nrepl                      "0.10.2"]
                 [ch.qos.logback/logback-classic         "1.0.13"]
                 ])
