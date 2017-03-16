(ns scratchpad.scratch
  (:require
   [clojure.tools.logging   :as log]
   [clojure.core            :as core]
   [clj-etl-utils.sequences :as etl-seq]
   [scratchpad.heap         :as heap]
   [clojure.java.io         :as io]
   [schema.core             :as s]
   [com.rpl.specter         :as specter])
  (:import
   [scratchpad.heap BinHeap]))


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
