(ns scratchpad.scratch
  (:require
   [clojure.tools.logging         :as log]
   [clojure.core                  :as core]
   [clj-etl-utils.sequences       :as etl-seq]
   [clj-etl-utils.landmark-parser :as lp]
   [scratchpad.heap               :as heap]
   [clojure.java.io               :as io]
   [schema.core                   :as s]
   [clojure.math.combinatorics    :as combo]
   [com.rpl.specter               :as specter]
   [clojure.data.json             :as json]
   [clojure.string                :as string])
  (:import
   [scratchpad.heap BinHeap]
   [org.jsoup Jsoup]))



(comment
  (log/infof "test log.info: %s" (java.util.Date.))

  (combo/permutations [:apple :orange :banana])
  ((:apple :orange :banana)
   (:apple :banana :orange)
   (:orange :apple :banana)
   (:orange :banana :apple)
   (:banana :apple :orange)
   (:banana :orange :apple))

  (sort [:apple :orange :banana])
  (:apple :banana :orange)

  (combo/permutation-index [:apple :banana :orange])
  0

  (combo/permutation-index [:apple :orange :banana])
  1

  (combo/permutation-index [:orange :banana :apple])
  5


  )


(defonce words-file "/usr/share/dict/words")
(def num-words (memoize (fn []
                          (with-open [rdr (io/reader words-file)]
                            (count (line-seq rdr))))))

(defn random-words [n]
  (with-open [rdr (io/reader words-file)]
    (doall
     (etl-seq/random-sample-seq
      (line-seq rdr)
      (num-words)
      n))))

(defn test-seq [tname op s]
  (spit
   (str tname ".dot")
   (->
    (BinHeap. op)
    (heap/insert-all s)
    heap/heap->pairs
    heap/pairs->digraph)))

(defn test-seq->heap [op s]
  (->
   (BinHeap. op)
   (heap/insert-all s)))

(comment

  (test-seq
   "h5"
   <= ;; >=
   (->>
    (repeatedly rand)
    (map #(* 10000 %))
    (map int)
    (take 25)))


  (test-seq
   "w1"
   (fn [a b]
     (= -1 (.compareToIgnoreCase a b)))
   (random-words 25))

  (test-seq
   "w2"
   (fn [a b]
     (not= -1 (.compareToIgnoreCase a b)))
   (random-words 25))


  )

(def UserAccount
  {(s/required-key :full-name) s/Str
   (s/required-key :email)     s/Str
   (s/required-key :id)        s/Num
   (s/required-key :props)     {s/Keyword s/Str}})


(comment


  (require '[schema-generators.complete :as c] '[schema-generators.generators :as g])

  (schema-generators.generators/sample 3 UserAccount)

  ({:full-name "", :email "", :id 0.5, :props {:! ""}}
   {:full-name "k", :email "", :id -1, :props {:q:4 "F"}}
   {:full-name "Zi", :email "", :id -1, :props {:fR1 ""}})


  )


(comment

  (->
   (java.io.File/createTempFile "this" "")
   str)
  "/var/folders/dm/j2p8cll146q39384tjcf8_9dpk4_s1/T/this8093560198764234544"

  ;; (import 'com.google.common.io.Files)

  (com.google.common.io.Files/toString
   (java.io.File. "/etc/passwd")
   (java.nio.charset.Charset/forName "UTF-8"))

  (import java.nio.charset.Charset)
  (import java.util.Date)

  (com.google.common.io.Files/write
   "this is my\ncontent\nok\ngot it?"
   (java.io.File. "some.file")
   (java.nio.charset.Charset/forName "UTF-8"))


  ;; Files.write(this.config.get(???), new File(certPath, "key.pem"), CharSet.forName())

  (str (java.io.File. "/some/path/to/a/file.txt"))

  (let [props (java.util.Properties.)]
    (with-open [rdr (java.io.FileInputStream. "some.properties")]
      (.load props rdr))
    props
    #_(into {} props))

  ;; DockerClient dockerClient = DockerClientBuilder.getInstance("http://localhost:2375").build();
  ;; (com.github.dockerjava.api.DockerClient/getInstance "http://192.168.99.100:2375")
  ;; (com.github.dockerjava.core.DockerClientBuilder/getInstance "tcp://192.168.99.100:2375")

  (let [config (com.github.dockerjava.core.DockerClientConfig/createDefaultConfigBuilder)]
    (.withUri config "https://192.168.99.100:2376")
    (.withDockerCertPath config "/Users/kburton/.docker/machine/certs")
    ;; (.withUsername config registryUser)
    ;; (.withPassword config registryPass)
    ;; (.withEmail config registryMail)
    ;; (.withServerAddress config registryUrl)
    (.build config))

  )


(comment


  (.getResource Class "/foo.foo")

  (slurp (.getResourceAsStream Class "/foo.foo"))


  (+ 3 4)

  (->
   (doto (java.util.HashMap.)
     (.put "this" :that)
     (.put "other" "thing")
     (.put "111" 222)
     (.put "111" {"more" 234
                  "stuff" 3.1415777}))
   )
  )


(defn tell-isaac-hi [msg]
  (.println System/out (format "Yo Isaac: %s" msg)))


(defn braces-recursive [^String s]
  (let [opens-for-closes {"{" "}"
                          "[" "]"
                          "(" ")"}
        is-open?         #{"(" "{" "["}]
    (loop [[ch & chars] (.split s "")
           expect       []]
      (cond
        (not ch)
        (empty? expect)

        (is-open? ch)
        (recur chars (cons (opens-for-closes ch) expect))

        (= (first expect) ch)
        (recur chars (rest expect))

        :otherwise
        false))))

(comment
  (braces-recursive "(")
  (braces-recursive "))")
  (braces-recursive "()")
  (braces-recursive "(){}[]")
  (braces-recursive "({)}")
  (braces-recursive "((({{{[[[((()))]]]}}})))")

  )


(comment

  (specter/setval [specter/MAP-KEYS specter/NAMESPACE] (str *ns*) any-map)

  (specter/transform [specter/ALL :a even?] inc [{:a 1} {:a 2 :b 1} {:a 4}])

  ;; [{:a 1} {:a 3, :b 1} {:a 5}]

  ;; => [{:a 1} {:a 3, :b 1} {:a 5}]


  )


(comment

  (def wikipedia-url "https://en.wikipedia.org/wiki/Comparison_of_network_monitoring_systems")
  (def content (slurp wikipedia-url))

  (count content)
  ;; => 126853

  (def doc (lp/make-parser content))

  (def table-content
    (lp/extract
     doc
     [[:ft "<table class=\"wikitable sortable"]]
     [[:ft "</table>"]]))

  (def rows
    (..
     (Jsoup/parse table-content)
     (select "table.wikitable")
     (get 0)
     (select "tr")))

  (def header (.get rows 0))

  ;; the first and last of each row are headers
  ;; from each row, need to grab the th then the trs



  (def chars "01000010 01000101 01000101 01010010")


  (let [chars (->
               "01000010 01000101 01000101 01010010"
               (.split " ")
               vec)
        nums  (mapv #(Integer/parseInt % 2)
                    chars)]
    (mapv #(-> % char str) nums))



  (+ 3 1)

  )


(comment

  ;; practicing a bit with the content in:
  ;;     https://clojure.org/guides/threading_macros

  ;; butlast? how have I never seen you before? NICE!
  (butlast [1 2 3])
  ;; (1 2)

  )


(defn describe-number [nn]
  (cond-> []
    (odd? nn)  (conj "odd")
    (even? nn) (conj "even")
    (pos? nn)  (conj "positive")
    (neg? nn)  (conj "negative")))

(comment

  (describe-number 0)   ;; ["even"]
  (describe-number 3)   ;; ["odd" "positive"]
  (describe-number 2)   ;; ["even" "positive"]
  (describe-number -3)  ;; ["odd" "negative"]
  (describe-number -2)  ;; ["even" "negative"]
  (describe-number Integer/MAX_VALUE)  ;; ["odd" "positive"]
  (describe-number Integer/MIN_VALUE)  ;; ["even" "negative"]
  (describe-number Long/MAX_VALUE)     ;; ["odd" "positive"]
  (describe-number Long/MIN_VALUE)     ;; ["even" "negative"]

  (describe-number Double/MAX_VALUE) ;; BOOM


  )



(comment


  (slurp "/home/kyle/x.x")
  (def col-data "...")

  (def tz-cols
    (as-> col-data it
      (.split ^String it "\n")
      (filter #(not (.contains ^String % "_raw")) it)
      (vec it)
      (drop 2 it)
      (mapv #(vec (.split ^String % "\\s*\\|\\s*")) it)
      (mapv #(cons "tix" (drop 1 %)) it)))

  (with-open [wtr (io/writer (io/file "/home/kyle/y.y"))]
    (doseq [[schema tname cname] tz-cols]
      (.write wtr (format "ALTER TABLE %-25s ALTER COLUMN %-25s SET DATA TYPE timestamp without time zone;\n"
                          (format "%s.%s" schema tname) cname))))

  )


(defn rand-double []
  (.nextDouble (java.util.Random.)))

(defn rand-grade []
  (+ 1 (* 3 (rand-double))))




(comment
  (with-open [wtr (io/writer "Foo.java")]
    (doseq [[fname lname]
            [["Robert" "Plant"]
             ["David"  "Lee-Roth"]
             ["Phill"  "Collins"]
             ["David"  "Byrne"]]]
      (.write wtr (format "this.studentList.add(new Student(\"%s\", \"%s\", %.2f, %.2f, %.2f, %.2f, %.2f));\n"
                          fname
                          lname
                          (rand-grade)
                          (rand-grade)
                          (rand-grade)
                          (rand-grade)
                          (rand-grade)))))

  )

(defn parse-project-file [project-file]
  (let [[_ _project-name _version-string & pairs] (-> project-file slurp read-string)
        project-info (apply hash-map pairs)]
    project-info))

(comment

  (def versions
    (->
     "https://raw.githubusercontent.com/cognitect-labs/aws-api/master/latest-releases.edn"
     slurp
     read-string))

  (spit
   "foo.foo"
   (vec
    (concat
     [(-> versions :api first)
      (-> versions :endpoints first)]
     (-> versions :services))))

  (with-open [wtr (io/writer "foo.foo")]
    (doseq [ent (vec
                 (concat
                  [(-> versions :api first)
                   (-> versions :endpoints first)]
                  (-> versions :services)))]
      (.write wtr (str ent))
      (.write wtr "\n")))


  (let [;; grab all the project dependencies
        dependencies (->
                      (parse-project-file "project.clj")
                      :dependencies)
        ;; filter for those that start with com.cognitect.aws
        aws-deps     (filter (fn [[dep _ver]]
                               (.startsWith (str dep) "com.cognitect.aws")) dependencies)
        ;; look up the new versions
        updated-aws-deps (reduce
                          (fn [acc [aws-dep _ver]]
                            (let [new-dep-info (get versions aws-dep)]
                              (conj acc [aws-dep (get new-dep-info ':mvn/version)])))
                          {}
                          aws-deps)]
    (with-open[wtr (io/writer "foo.foo")]
      (doseq [ent updated-aws-deps]
        (.write wtr (str ent))
        (.write wtr "\n")))
    updated-aws-deps)
  )


(comment


  (def wikipedia-us-states (slurp "/home/kyle/Downloads/List of U.S. state and territory abbreviations - Wikipedia.html"))
  (count wikipedia-us-states) ;; 481633

  (def doc (lp/make-parser wikipedia-us-states))

  (def main-table
    (lp/extract
     (lp/make-parser wikipedia-us-states)
     [[:ft "<table class=\"wikitable"]
      [:ft "<span class=\"flagicon\">"]
      [:rt "<tbody>"]]
     [[:ft "</tbody>"]]))

  (.substring main-table 0 10000)

  (defn extract-state-code [[name-of-region _status-of-region _iso-code _ansi-code _ansi-num usps _usgc _gpo _ap _other-abbreviations]]
    (if (.contains ^String usps "<")
      [name-of-region (-> ^String usps (.split "<" 2) first)]
      [name-of-region usps]))

  (def rows
    (lp/html-table->matrix main-table))

  (def state-codes
   (->>
    rows
    (map extract-state-code)
    (filter #(not (empty? %)))))

  (spit "/home/kyle/us-state-codes.txt"
        (string/join "\n" (sort state-codes)))


  )
