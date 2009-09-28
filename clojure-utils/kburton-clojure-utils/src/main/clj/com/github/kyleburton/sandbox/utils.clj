(ns com.github.kyleburton.sandbox.utils
  (:import  [java.io PrintWriter BufferedWriter Writer OutputStream OutputStreamWriter File FileOutputStream]
            [java.net URL URI MalformedURLException])
  (:require [clojure.contrib.duck-streams        :as ds]
            [clojure.contrib.shell-out           :as sh]))

(defn raise
  "Simple wrapper around throw."
  [& args]
  (throw (RuntimeException. (apply format args))))

(defn perror [& args]
  (.println System/err (apply format args)))

(defn log [& args]
  (.println System/err (apply format args)))

(defn uc [#^String s] (.toUpperCase s))
(defn lc [#^String s] (.toLowerCase s))

(defn #^String get-user-home
  "Get the user's home dir as a string."
  []
  (System/getProperty "user.home"))

(defn #^java.io.File ->file
  "Coerce into a File."
  [thing]
  (java.io.File. (str thing)))

(defmacro with-tmp-file [[var & [prefix suffix]] & body]
  `(let [prefix# ~prefix
         suffix# ~suffix
         ~var (java.io.File/createTempFile (or prefix# "pfx") (or suffix# "sfx"))]
     ~@body))

(defmacro with-tmp-dir [[var & [prefix suffix]] & body]
  `(let [prefix# ~prefix
         suffix# ~suffix
         ~var (java.io.File/createTempFile (or prefix# "pfx") (or suffix# "sfx"))]
     (try
      (do
        (.delete ~var)
        ~@body)
      (finally
       ;; TODO: this will fail if dir is not empty!, should this recrusively remove all the files?
       (.delete ~var)))))

(defn basename
  "Strip off the last part of the file name."
  [fname]
  (if (instance? java.io.File fname)
    (.getParent fname)
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
        :else
        path))


(defn mkdir
  "Create the given directory path, fall back gracefuly if the path
  exists, warning if it's not a directory."
  [path]
  (let [f (->file path)]
    (if (not (.exists f))
      (do
        ;(log "[INFO] mkdir: creating %s" path)
        (.mkdirs f)
        true)
      (if (not (.isDirectory f))
        (do
          ;(log "[WARN] mkdir: %s exists and is not a directory!" path)
          false)
        (do
          ;(log "[DEBUG] mkdir: exists: %s" path)
          true)))))

(defmulti  exists? class)
(defmethod exists? String   [s] (.exists (File. s)))
(defmethod exists? File     [f] (.exists f))
(defmethod exists? :default [x] (throw (Exception. (str "Do not know how to test <" (pr-str x) "> if it `exists?'"))))


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
   (if (instance? Class (class thing))
     (seq (.getDeclaredMethods thing))
     (seq (.getDeclaredMethods (class thing))))))

(defn fields-seq
  ([thing]
   (if (instance? Class (class thing))
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
   (if (instance? Class (class thing))
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
  (let [tclass (if (instance? Class (class thing))
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

;; clojure.lang.PersistentVector$Node ins't serializable any longer...is this an oversight? ignore for now...
(defn freeze [obj]
  (with-open [baos (java.io.ByteArrayOutputStream. 1024)
              oos  (java.io.ObjectOutputStream. baos)]
    (.writeObject oos obj)
    (.toByteArray baos)))

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

(defn string->sha1 [s]
  (sha1->string (.getBytes s)))

(defn string->md5 [s]
  (md5->string (.getBytes s)))




;; console is not available through slime :(, but X is - try using swing/get-password-dialog instead
;; (String. (.readPassword console "[%s]", (into-array ["password"])))


(defn parse-paired-arglist
  "Ensures the given set of key/value pairs is a map, extracting non
'pair' arguments into an additional list.  Eg:

  (parse-paired-arglist [:foo 1 :bar 2])     => [[]    {:bar 2, :foo 1}]
  (parse-paired-arglist [:foo 1 :bar 2])     => [[]    {:bar 2, :foo 1}]
  (parse-paired-arglist [:foo 1 3 4 :bar 2]) => [[3 4] {:bar 2, :foo 1}]

This is most useful in functions where you want to be able to take
sets of optional parameters:

  (defn my-func [arg1 & params]
    (let [[additional args] (parse-paired-arglist params)
          args (merge {:foo \"default\" :bar \"default\"} args)]
      (prn (format \"foo=%s; bar=%s; qux=%s\"
                   (:foo args)
                   (:bar args)
                   (:qux args)))))
  (my-func 1)                      => \"foo=default; bar=default; qux=null\"
  (my-func 1 :foo 2)               => \"foo=2; bar=default; qux=null\"
  (my-func 1 :bar 3 :qux 4)        => \"foo=default; bar=3; qux=4\"
  (my-func 1 :foo 2 :bar 3 :qux 4) => \"foo=2; bar=3; qux=4\"
"
  [args]
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

(defn ensure-directory
  "Create the directory if it does not already exist."
  [dir]
  (let [f (java.io.File. dir)]
    (if (not (.exists f))
      (.mkdirs f))))

;; TODO: port to pure java, rm is unix specific...
(defn deltree
  "Remove the given directory tree, all files and subdirectories."
  [dir]
  (sh/sh "rm" "-rf" dir))

(defn enumeration->seq [#^java.util.Enumeration enum]
  (loop [has-more (.hasMoreElements enum)
         res []]
    (if has-more
      (let [elt (.nextElement enum)]
        (recur (.hasMoreElements enum) (conj res elt)))
      res)))

(defn properties->map [& [#^java.util.Properties props]]
  (reduce (fn [m ent] (assoc m
                        (.getKey ent)
                        (.getValue ent)))
          {}
          (iterator-seq (.iterator (.entrySet (or props (System/getProperties)))))))

(comment

  (doseq [[p v] (properties->map)]
    (println (format "%s: %s" p v)))

)


(defn now-milliseconds []
  (.getTime (java.util.Date.)))

(defn string-gzip [#^String s]
  (with-open [bout (java.io.ByteArrayOutputStream.)
              gzout (java.util.zip.GZIPOutputStream. bout)]
    (.write gzout (.getBytes s))
    (.finish gzout)
    (.toByteArray bout)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: offer these back to duck-streams...

(defmulti appender class)

(defmethod appender PrintWriter [x] x)

(defmethod appender BufferedWriter [#^BufferedWriter x]
  (PrintWriter. x))

(defmethod appender Writer [x]
  ;; Writer includes sub-classes such as FileWriter
  (PrintWriter. (BufferedWriter. x)))

(defmethod appender OutputStream [x]
  (PrintWriter.
   (BufferedWriter.
    (OutputStreamWriter. x "UTF-8"))))

(defmethod appender File [#^File x]
  (appender (FileOutputStream. x true)))

(defmethod appender URL [#^URL x]
  (if (= "file" (.getProtocol x))
    (appender (File. (.getPath x)))
    (throw (Exception. (str "Cannot write to non-file URL <" x ">")))))

(defmethod appender URI [#^URI x]
  (appender (.toURL x)))

(defmethod appender String [#^String x]
  (try (let [url (URL. x)]
         (appender url))
       (catch MalformedURLException err
         (appender (java.io.File. x)))))

(defmethod appender :default [x]
  (throw (Exception. (str "Cannot open <" (pr-str x) "> as an appender."))))

(defmacro with-out-appender
  "Opens an appender on f, binds it to *out*, and evalutes body."
  [f & body]
  `(with-open [stream# (appender ~f)]
     (binding [*out* stream#]
       ~@body)))

;; END duck-streams contrib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
