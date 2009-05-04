;; fetch dependencies and set things up

(defn raise [args]
  (throw (RuntimeException. (apply format args))))

(defn log [& args]
  (prn (apply format args)))

(defn ->file [thing]
  (java.io.File. (str thing)))

(defn $HOME [& paths]
  (->file (apply str (cons (str (System/getProperty "user.home") "/") (apply str (interpose "/" paths))))))

(defn mkdir [path]
  (let [f (->file path)]
    (if (not (.exists f))
      (do
        (log "[INFO] mkdir: creating %s" path)
        (.mkdirs f))
      (if (not (.isDirectory f))
        (log "[WARN] mkdir: %s exists and is not a directory!")
        (log "[DEBUG] mkdir: exists: %s" path)))))

(defn drain-line-reader [rdr]
  (loop [res []
         line (.readLine rdr)]
    (if line
      (recur (conj res line)
             (.readLine rdr))
      res)))

(defn exec [cmd]
  (let [proc (.exec (Runtime/getRuntime) cmd)
        rv (.waitFor proc)]
    {:error (drain-line-reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getErrorStream proc))))
     :output (drain-line-reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream proc))))
     :exit rv}))

(defn symlink [src dst]
  (let [src (->file src)
        dst (->file dst)]
    (if (not (.exists src))
      (raise "symlink: src does not exist: %s" src))
    (if (.exists dst)
      (log "[INFO] symlink: dst exists %s => %s" src dst)
      (let [cmd (format "ln -s %s %s" src dst)
            res (exec cmd)]
        (log "[INFO] symlink: %s=>%s : %s" src dst cmd)
        (if (not (= 0 (:exit res)))
          (log "[ERROR] %s" (:error res)))))))

(defn delete [path]
  (let [path (->file path)]
    (if (.exists path)
      (.delete path))))

(def *deps-location*  "http://asymmetrical-view.com/personal/repo/")

(defn url-get [url]
  (with-open [is (.openStream (java.net.URL. url))]
    (loop [sb (StringBuffer.)
           chr (.read is)]
      (if (= -1 chr)
        sb
        (do
          (.append sb (char chr))
          (recur sb
                 (.read is)))))))

(defn url-download [url target-dir]
  (let [cmd (format "wget -P %s -c %s" target-dir url)
        res (exec cmd)]
    (log "[INFO] wget: %s" cmd)
    (if (not (= 0 (:exit res)))
      (log "[ERROR] %s" (:error res)))))


(defn- all-groups [m]
  (for [grp (range 1 (+ 1 (.groupCount m)))]
    (.group m grp)))


(defn re-find-all [re str]
  (doall
   (loop [m (re-matcher re str)
          res []]
     (if (.find m)
       (recur m (conj res (vec (all-groups m))))
       res))))

(defn list-deps []
  (filter #(.endsWith % ".jar") 
          (map #(nth % 0)
               (re-find-all #"href=\"(.+?)\">" (.toString (url-get *deps-location*))))))


(def *target-dir* ($HOME "/.clojure"))

(defn get-deps []
  (doseq [dep (list-deps)]
    (if (.exists ($HOME ".clojure/" dep))
      (log "[INFO] already have: %s" dep)
      (do
        (log "[INFO] getting: %s from %s => %s" dep (str *deps-location* dep) ($HOME ".clojure/" dep))
        (url-download (str *deps-location* dep)
                      ($HOME ".clojure/"))))))

(defn chmod [perms file]
  (let [cmd (format "chmod %s %s" perms file)
        res (exec cmd)]
    (log "[INFO] chmod: %s" cmd)
    (if (not (= 0 (:exit res)))
      (log "[ERROR] %s" (:error res)))))

(defn make-clojure-bin [fnum port]
  (let [target ($HOME (format "bin/clojure%s" fnum))]
    (if (.exists target)
      (log "[INFO] already exists: %s" target)
      (with-open [outp (java.io.PrintWriter. target)]
        (binding [*out* outp]
          (print (str "
CLOJURE_JAR=\"$HOME/.clojure/clojure.jar\"
CONTRIB_JAR=\"$HOME/.clojure/clojure-contrib.jar\"

CLASSPATH=\"$CLOJURE_JAR:$CONTRIB_JAR\" 

for f in $HOME/.clojure/*; do
    CLASSPATH=\"$CLASSPATH:$f\"
done

java -server \\
  -Xdebug \\
  -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=" port " \\
  -cp \"$CLASSPATH\" \\
  clojure.lang.Repl \\
  \"$@\"
")))))
    (chmod 755 target)))

(defn main []
  (mkdir ($HOME "/.clojure"))
  (symlink  ($HOME "/personal/projects/sandbox/clojure-utils/src/clj/") ($HOME "/.clojure/krb-clj-utils"))
  (get-deps)
  (make-clojure-bin "" 8888)
  (make-clojure-bin "2" 8889)
  (make-clojure-bin "3" 8890))


;; (main)
