(ns com.github.kyleburton.sandbox.utils
  (:require [clojure.contrib.duck-streams :as ds]))

(defn raise 
  "Simple wrapper around throw."
  [args]
  (throw (RuntimeException. (apply format args))))

(defn log [& args]
  (prn (apply format args)))

(defn #^String get-user-home
  "Get the user's home dir as a string."
  []
  (System/getProperty "user.home"))

(defn #^java.io.File ->file
  "Coerce into a File."
  [thing]
  (java.io.File. (str thing)))

(defn basename [fname]
  (cond (isa? fname java.io.File)
        (.getParent fname)
        true
        (.getParent (java.io.File. (str fname)))))


(defn #^java.io.File $HOME
  "Construct a path relative to the user's home directory."
  [& paths]
  (->file (apply str 
                 (cons (str (System/getProperty "user.home") "/") 
                       (apply str (interpose "/" paths))))))

(defmulti expand-file-name
  "Perform bash style expansion on the given path.  Eg: ~/file.txt."
  class)

(defmethod expand-file-name String [#^String path]
  (cond (.startsWith path "~/")
        (.replaceFirst path "^~(/|$)" (str (get-user-home) "/"))
        (.startsWith path "file://~/")
        (.replaceFirst path "^file://~/" (str "file://" (get-user-home) "/"))
        true
        path))


(defn mkdir
  "Create the given directory path, fall back gracefuly if the path
  exists, warning if it's not a directory."
  [path]
  (let [f (->file path)]
    (if (not (.exists f))
      (do
        (log "[INFO] mkdir: creating %s" path)
        (.mkdirs f)
        true)
      (if (not (.isDirectory f))
        (log "[WARN] mkdir: %s exists and is not a directory!")
        (log "[DEBUG] mkdir: exists: %s" path)))))

(defn drain-line-reader 
  "Drain a buffered reader into a sequence."
  [#^java.io.BufferedReader rdr]
  (loop [res []
         line (.readLine rdr)]
    (if line
      (recur (conj res line)
             (.readLine rdr))
      res)))

(defn exec
  "Simple wrapper around Runtime.exec - not intended to compete with clojure.contrib.shell-out"
  [cmd]
  (let [proc (.exec (Runtime/getRuntime) cmd)
        rv (.waitFor proc)]
    {:error (drain-line-reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getErrorStream proc))))
     :output (drain-line-reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream proc))))
     :exit rv}))

(defn symlink
  "Create a symlink."
  [src dst]
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

(defn delete
  "Remove a file if it exists."
  [path]
  (let [path (->file path)]
    (if (.exists path)
      (.delete path))))

(defn url-get 
  "Very simplistic retreival of a url target."
  [url]
  (with-open [is (.openStream (java.net.URL. url))]
    (loop [sb (StringBuffer.)
           chr (.read is)]
      (if (= -1 chr)
        sb
        (do
          (.append sb (char chr))
          (recur sb
                 (.read is)))))))

(defn url-download
  "Shell's out to wget to pull the file into the target directory."
  [url #^String target-dir]
  (let [cmd (format "wget -P %s -c %s" target-dir url)
        res (exec cmd)]
    (log "[INFO] wget: %s" cmd)
    (if (not (= 0 (:exit res)))
      (log "[ERROR] %s" (:error res)))))


(defn all-groups 
  "Extracts all the groups from a java.util.regex.Matcher into a seq."
  [#^java.util.regex.Matcher m]
  (for [grp (range 1 (+ 1 (.groupCount m)))]
    (.group m grp)))


(defn re-find-all
  "Retreive all of the matches for a regex in a given string."
  [re str]
  (doall
   (loop [m (re-matcher (if (isa? (class re) String) (re-pattern re) re) str)
          res []]
     (if (.find m)
       (recur m (conj res (vec (all-groups m))))
       res))))

(defn re-find-first
  "Retreive the first set of match groups for a regex in a given string."
  [re str]
  (first
   (doall
    (loop [m (re-matcher (if (isa? (class re) String) (re-pattern re) re) str)
           res []]
      (if (.find m)
        (recur m (conj res (vec (all-groups m))))
        res)))))

(defn chmod
  "Change a file or directory's permissions.  Shells out to perform the chmod."
  [perms file]
  (let [cmd (format "chmod %s %s" perms file)
        res (exec cmd)]
    (log "[INFO] chmod: %s" cmd)
    (if (not (= 0 (:exit res)))
      (log "[ERROR] %s" (:error res)))))


;; reflection class and doc utils
(defn methods-seq
  ([thing]
   (if (= Class (class thing))
     (seq (.getDeclaredMethods thing))
     (seq (.getDeclaredMethods (class thing))))))

(defn fields-seq
  ([thing]
   (if (= Class (class thing))
     (seq (.getDeclaredFields thing))
     (seq (.getDeclaredFields (class thing))))))

(defn fields-value-seq
  ([thing]
     (map #(.get % thing) (fields-seq thing))))

(defn fields-and-values-seq
  ([thing]
     (for [field (fields-seq thing)]
       [(.getName field)
        (.get field thing)])))

;; this is not the same as 'bean' - bean doesn't grab fields, this
;; grabs only fields...
(defn fields-and-values-map
  ([thing]
     (reduce 
      (fn [m [k v]]
        (assoc m (keyword k) v))
      {}
      (fields-and-values-seq thing))))

(defn constructors-seq
  ([thing]
   (if (= Class (class thing))
     (seq (.getConstructors thing))
     (seq (.getConstructors (class thing))))))

(defn method-modifiers-as-strings [#^java.lang.reflect.Method method]
  (str (.getModifiers method)))

(defn method-return-type [#^java.lang.reflect.Method method]
  (.getReturnType method))

(defn method-name-short [#^java.lang.reflect.Method method]
  (.getReturnType method))

(defn method-argument-list [#^java.lang.reflect.Method method]
  (.getParameterTypes method))

(defn short-method-sig [#^java.lang.reflect.Method method]
  (str (method-modifiers-as-strings method)
        " "
       (method-return-type method)
        " "
       (method-name-short method)
        "("
       (method-argument-list method)
        ")"))

(defn make-subst-fn [pattern replacement]
  (fn [str]
      (.replaceAll (.toString str)
                    pattern replacement)))


;; TODO: add in abstract/interface/class info, then list all parent
;; classes and interfaces
(defn doc-class [thing]
  "Prints (to *out*) a summary of the class, it's members and its
methods."
  (let [tclass (if (= Class (class thing))
                 (identity thing)
                 (class thing))
        trimmer (make-subst-fn "java.lang." "")]
    (println (format "Class %s (%s)" (.getName tclass) (.getSuperclass tclass)))
  (println (format "  Interfaces:"))
  (doseq [interf (seq (.getInterfaces tclass))]
    (println (str "    " (trimmer interf))))
  (println (str "  Constructors:"))
  (doseq [constructor (constructors-seq tclass)]
    (println (str "    " (trimmer constructor))))
  (println (str "  Members:"))
  (doseq [field (fields-seq tclass)]
    (println (str "    " (trimmer field))))
  (println (str "  Methods:"))
  (doseq [method (methods-seq tclass)]
    (println (str "    " (trimmer method))))))


(defn object->file [obj file]
  (with-open [outp (java.io.ObjectOutputStream. (java.io.FileOutputStream. file))]
    (.writeObject outp obj)))


(defn file->object [file]
  (with-open [inp (java.io.ObjectInputStream. (java.io.FileInputStream. file))]
    (.readObject inp)))

(defn freeze 
  ([obj]
     (with-open [baos (java.io.ByteArrayOutputStream. 1024)
                 oos  (java.io.ObjectOutputStream. baos)]
       (.writeObject oos obj)
       (.toByteArray baos)))
  ([obj & objs]
     (freeze (vec (cons obj objs)))))

;; (freeze "foo")
;; (freeze "foo" "bar" "qux")


(defn thaw [bytes]
  (with-open [bais (java.io.ByteArrayInputStream. bytes)
              ois  (java.io.ObjectInputStream. bais)]
    (.readObject ois)))

;; (thaw (freeze "foo"))

;; (object->file "foo" ($HOME "/foo.bin"))
;; (file->object ($HOME "/foo.bin"))

(defmacro with-stdout-to-file [file & body]
  `(with-open [out# (ds/writer ~file)]
     (binding [*out* out#]
       ~@body)))

(defmacro with-stderr-to-file [file & body]
  `(with-open [out# (ds/writer ~file)]
     (binding [*err* out#]
       ~@body)))

(defn pairs->map [pairs]
  (if (not (even? (count pairs)))
    (throw (RuntimeException. (format "Error, pairs->map on odd # of fields? %d:(%s)" (count pairs) pairs)))
    (reduce (fn [m [k v]] (assoc m k v))
            {}
            (partition 2 pairs))))


(defn md5->string [bytes]
  (let [digester (java.security.MessageDigest/getInstance "MD5")]
    (.update digester bytes)
    (apply str (map (fn [byte]
                      (Integer/toHexString (bit-and 0xFF byte)))
                    (.digest digester)))))

(defn sha1->string [bytes]
  (let [digester (java.security.MessageDigest/getInstance "SHA1")]
    (.update digester bytes)
    (apply str (map (fn [byte]
                      (Integer/toHexString (bit-and 0xFF byte)))
                    (.digest digester)))))

;; (md5->string (.getBytes "foo bar\n"))
;; (sha1->string (.getBytes "foo bar\n"))






;; console is not available through slime :(, but X is - try using swing/get-password-dialog instead
;; (String. (.readPassword console "[%s]", (into-array ["password"])))


(defn parse-paired-arglist [args]
  (if (map? args)
    [[] args]
    (loop [res {} 
           unnamed []
           [arg & args] args]
      (if (not arg)
        [unnamed res]
        (if (keyword? arg)
          (recur (assoc res arg (first args))
                 unnamed
                 (rest args))
          (recur res
                 (conj unnamed arg)
                 args))))))

;; (parse-paired-arglist '[:foo bar this that :other thing])
;; (parse-paired-arglist {:foo 'bar :other 'thing})

