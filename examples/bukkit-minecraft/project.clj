(defproject minerepl "0.1.0-SNAPSHOT"
  :description       "FIXME: write description"
  :url               "http://example.com/FIXME"
  :license           {:name "Eclipse Public License"
                      :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :repl-options      {:init-ns kbot-bukkit.core}
  :source-paths      ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :profiles          {:dev {:dependencies [[org.typedclojure/typed.clj.checker "1.0.31"]]}}
  :main              kbot-bukkit.core
  :dependencies      [[org.clojure/clojure                    "1.11.1"]
                      [org.typedclojure/typed.clj.runtime     "1.0.31"]
                      [org.clojure/tools.logging              "1.2.4"]
                      [nrepl/nrepl                            "0.9.0"]
                      [cider/cider-nrepl                      "0.28.5"]
                      [ch.qos.logback/logback-classic         "1.2.11"]
                      [org.clojure/data.json                  "2.4.0"]
                      ;; https://mvnrepository.com/artifact/org.spigotmc/spigot-api
                      [org.spigotmc/spigot-api                "1.9"]

                      ])
