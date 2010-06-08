(ns leiningen.run
  (:require [clojure.contrib.str-utils :as str-utils]
            [clojure.contrib.shell-out :as shell])
  (:import
   [java.io BufferedReader InputStreamReader OutputStreamWriter PrintWriter]))

(defn files-in-dir [p re]
  (filter (fn [f]
            (.matches (.getName f) re))
          (.listFiles (java.io.File. p))))

(defn exec [cmd]
  (let [proc (.exec (Runtime/getRuntime) cmd)
        inn  (PrintWriter. (OutputStreamWriter. (.getOutputStream proc)))  ;; stdin
        err  (BufferedReader. (InputStreamReader. (.getErrorStream proc))) ;; stderr
        out  (BufferedReader. (InputStreamReader. (.getInputStream proc))) ;; stdout
        stdin (BufferedReader. (InputStreamReader. System/in))
        iwtr (Thread. (fn []
                        (loop [l (.readLine stdin)]
                          (if l
                            (do
                              (println (str "[stdin=>inn] " l))
                              (.println inn l)
                              (.flush inn)
                              (recur (.readLine stdin)))))))
        ordr (Thread. (fn []
                        (loop [l (.readLine out)]
                          (if l
                            (do
                              (println l)
                              (recur (.readLine out)))))))
        erdr (Thread. (fn []
                        (loop [l (.readLine err)]
                          (if l
                            (do
                              (println l)
                              (recur (.readLine err)))))))]
    (.setDaemon ordr true)
    (.start ordr)
    (.setDaemon erdr true)
    (.start erdr)
    (.setDaemon iwtr true)
    (.start iwtr)
    (.waitFor proc)))

(defn project-classpath [p]
  (str-utils/str-join
   (System/getProperty "path.separator")
   (cons
    (str (:root p) (System/getProperty "file.separator") (:name p) ".jar")
    (map str
         (files-in-dir (:library-path p) ".+\\.jar$")))))

(defn run-as-script [project to-run & args]
  (let [cmd (into-array String (concat ["java" "-server" "-cp" (project-classpath project)
                                        "clojure.main" to-run]
                                       args))]
    (exec cmd)))

(defn run-as-class [project to-run & args]
  (let [cmd (into-array String (concat ["java" "-server" "-cp" (project-classpath project)
                                        to-run]
                                       args))]
    (exec cmd)))

(defn run [project to-run & args]
  (if (.exists (java.io.File. to-run))
    (apply run-as-script project to-run args)
    (apply run-as-class  project to-run args)))


