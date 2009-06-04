(ns com.github.kyleburton.sandbox.jar-utils
  (:import [java.io File])
  (:use    [com.github.kyleburton.sandbox.utils :as kutils]
           [clojure.contrib.shell-out           :as sh]
           [clojure.contrib.str-utils           :as str-utils]
           [clojure.contrib.duck-streams        :as ds]))

(defn jar-files
  "Returns a sequence of the jar files from the given directory.  This is not recrusive."
  [#^String path]
  (doall (filter #(.endsWith (.toLowerCase (.getName %)) ".jar")
                 (filter #(.isFile %) (seq (.listFiles (File. path)))))))

(defn get-jar-certs [#^File jar-file]
  (sh/sh "jarsigner" "-certs" "-verbose" "-verify" (.toString jar-file)))

(def jar-signed? 
     (memoize
      (fn [#^File jar-file]
        (.contains (.toLowerCase (get-jar-certs jar-file)) "entry was signed on"))))

(defn unsign-manifest-content
  "Remove the signing information from the manifest content."
  [#^String manifest]
  (loop [[line & lines] (seq (.split manifest "\r?\n"))
         res []]
    (if (not line)
      ;; done
      (str-utils/str-join "\r\n" res)
      ;; more lines to process
      (do ;; this is what it looked like in an example...hope this is consistient
        (if (.startsWith line "Name: ")
          (recur (drop 1 (drop-while #(not (empty? %)) lines)) res)
          (recur lines (conj res line)))))))

(defn fix-manifest
  "Read, fix and rewrite the manifest file (removing signing information)."
  [#^String mf-file]
  (ds/spit mf-file (str (unsign-manifest-content (slurp mf-file)) "\r\n")))

(defn unsign-jar
  "Unarchive the jar, rewrite the manifest file, remove the
  RSA (public key) and SF (signature file), and recreate the jar
  file."
  [file working-dir target-dir]
  (if (not (jar-signed? file))
    (prn (format "[......] File: %s" file))
    (do
      (prn (format "[signed] File: %s" file))
      (let [unzip-dir     (str working-dir "/unzipped")
            manifest-file (format "%s/META-INF/MANIFEST.MF" unzip-dir)]
        (sh/sh "rm" "-rf" unzip-dir)
        (.mkdirs (File. unzip-dir))
        (println (sh/sh "jar" "xvf" 
                        (.toString file)
                        :dir unzip-dir))
        (println (sh/sh "rm" (format "%s/META-INF/PRIVKEY.RSA" unzip-dir)))
        (println (sh/sh "rm" (format "%s/META-INF/PRIVKEY.SF" unzip-dir)))
        (fix-manifest manifest-file)
        (prn (format "creating jar: jar cmf %s %s -C %s ./" 
                     manifest-file (.toString file)
                     unzip-dir))
        (println (sh/sh "jar" "cmf" manifest-file (.toString file) "-C" unzip-dir "./"))
        (prn (format "re-created: %s" file))))))


(defn unsign-jars
  "Unsing each jar file found in the given source directory, NB: this does not recurse into sub directories."
  [jars-src-dir jars-target-dir working-dir]
  (kutils/ensure-directory jars-target-dir)
  (if (not (.exists (File. working-dir)))
    (do
      (prn (format "copying %s => %s" jars-src-dir working-dir))
      (sh/sh "cp" "-r" jars-src-dir working-dir)))
  (doseq [file (take 2 (jar-files working-dir))]
    (unsign-jar file working-dir jars-target-dir)))
