(ns repl-from-java.core)

;; TODO: fill this ns with utilities for interacting w/your JVM based apps

(defn exit
  ([] (exit 0))
  ([code] (System/exit code)))

;; (exit 1)
