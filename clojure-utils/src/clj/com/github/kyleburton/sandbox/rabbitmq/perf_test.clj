(ns com.github.kyleburton.sandbox.rabbitmq.perf-test
  (:import [java.util Date]
           [java.io File])
  (:use [com.github.kyleburton.sandbox.rabbitmq :as rabbit]
        [com.github.kyleburton.sandbox.utils    :as kutils]
        [clojure.contrib.duck-streams           :as ds]
        [clojure.contrib.str-utils              :as str-utils]
        [incanter.core]
        [incanter.stats]
        [incanter.charts]))

;;
;; Benchmarking of AMQP 0.8 [1] Brokers
;;
;;  These tests are using the RabbitMQ java client library [2].  That
;;  client library supports the AMQP v0.8 standard, the impact of this
;;  is that the tests will only be executable against Brokers
;;  supporting the 0.8 version of the AMQP protocol.
;;
;;  The following brokers were considered for this benchmark:
;;
;;    1. RabbitMQ [3]
;; 
;;    2. ZeroMQ   [4]
;;
;;        Unable to benchmark: unable to determine how to configure it
;;        as an AMQP Broker, it is unclear to me at this time if
;;        ZeroMQ supports acting as an AMQP broker. The documentation
;;        implies that it can act as a client, but not a broker.
;;
;;    3. Apache Qpid (Java) [5]
;;
;;    4. Apache Qpid (C++)  [5]
;;
;;        Unable to benchmark: unable to successfuly build qpidc-0.5
;;
;;    5. OpenAMQ [6]
;;
;;       Unable to benchmark: OpenAMQ supports AMQP 0.9, this
;;       benchmark currently only supports AMQP 0.8 brokers.
;;
;; Hardware and Operating Systems:
;;
;;  System-A
;;   CPU:    Intel(R) Core(TM)2 Duo CPU     E8400  @ 3.00GHz
;;   Memory: 8G
;;   Linux:  Kubuntu 9.0.4
;;     Linux <<hostname>> 2.6.28-11-generic #42-Ubuntu SMP Fri Apr 17 01:58:03 UTC 2009 x86_64 GNU/Linux
;;
;; Java Version: 
;;   java version "1.6.0_12"
;;   Java (TM)
;;    SE Runtime Environment (build 1.6.0_12-b04)
;;
;;   Java HotSpot (TM)
;;    64-Bit Server VM (build 11.2-b01, mixed mode)
;;
;; Test Series 1: 
;;
;;    Run rounds of messages through the broker with the broker and
;;    client running on the same system.  This is using a simple
;;    direct binding between the exchange and the queue.  Single
;;    producer and a single consumer (as threads in the same JVM).
;;    The queue is durable the messages are not presistent.
;;
;;    1. Start the broker
;;    2. Start the JVM
;;    3. Execute the test three times recording statistics for each run.
;;    4. Shutdown jvm and broker before next series
;;
;;   Repeat the tests with 1, 2, 4, 8 and 16 producers, keeping the
;;   total message count fixed.
;;
;;   AMQP Broker: SystemA
;;   Benchmark:   SystemA
;;   
;; References:
;;    [1] http://jira.amqp.org/confluence/display/AMQP/Advanced+Message+Queuing+Protocol
;;    [2] http://www.rabbitmq.com/java-client.html
;;    [3] http://www.rabbitmq.com/
;;    [4] http://www.zeromq.org/
;;    [5] http://qpid.apache.org/
;;    [6] http://www.openamq.org/
;;
;; TODO: capture statistics from the broker:
;;    * memory usage profile
;;    * cpu utilization
;;    * disk io utiliziation
;;    * network io utiliziation
;;

(def *working-directory* (kutils/$HOME "/tmp/amqp-benchmark"))
(kutils/mkdir *working-directory*)

(defn to-tab [fields]
  (str-utils/str-join "\t" fields))

(defn to-tab* [& fields]
  (to-tab fields))

(def *testing-info* {:broker "RabbitMQ"
                     :series 1
                     :run-number 0
                     :num-producers  1
                     :num-consumers  1
                     :total-messages 1
                     :message-size   (count (kutils/freeze [11 (Date.)]))})

(def *producer-stats-file* (format "%s/producer-stats.tab" *working-directory*))

(defn ensure-producer-stats-file []
  (when (not (exists? *producer-stats-file*))
    (kutils/with-out-appender 
     *producer-stats-file*
     (println (to-tab*
               "TEST-SERIES"
               "RUN#"
               "PRODUCER#"
               "BROKER"
               "TOTAL-MESSAGES"
               "M/S"
               "MESSAGE-SIZE"
               "B/S"
               "START-TIME"
               "END-TIME"
               "ELAPSED-TIME")))))

(defn log-producer-stat [producer-num msgs-sent start-time end-time]
  (let [elapsed (- end-time start-time)]
    (kutils/with-out-appender
     *producer-stats-file*
     (println (to-tab* (:series         *testing-info*)
                       (:run-num        *testing-info*)
                       producer-num
                       (:broker         *testing-info*)
                       msgs-sent
                       (/ msgs-sent (/ elapsed 1000.0))
                       (:message-size *testing-info*)
                       (/ (* (:message-size *testing-info*) 
                             msgs-sent)
                          (/ elapsed 1000.0))
                       start-time
                       end-time
                       elapsed)))))

(def *consumer-stats-file* (format "%s/consumer-stats.tab" *working-directory*))

(defn ensure-consumer-stats-file []
  (when (not (exists? *consumer-stats-file*))
    (kutils/with-out-appender 
     *consumer-stats-file*
     (println (to-tab*
               "TEST-SERIES"
               "RUN#"
               "CONSUMER#"
               "BROKER"
               "TOTAL-MESSAGES"
               "M/S"
               "MESSAGE-SIZE"
               "START-TIME"
               "END-TIME"
               "ELAPSED-TIME"
               "MSG-TOTAL-AGE")))))

(defn log-consumer-stat [consumer-num msgs-sent start-time end-time msg-age]
  (let [elapsed (- end-time start-time)]
    (kutils/with-out-appender
     *consumer-stats-file*
     (println (to-tab* (:series         *testing-info*)
                       (:run-num        *testing-info*)
                       consumer-num
                       (:broker         *testing-info*)
                       msgs-sent
                       (/ msgs-sent (/ elapsed 1000.0))
                       (:message-size *testing-info*)
                       start-time
                       end-time
                       elapsed
                       msg-age)))))


(defn now
  "Fudge is a bit of a hack, 1ms shouldn't have any impact on the
benchmarks, when we use -1 in start-time's, it avoids divide by zero
exceptions when things happen so quickly that elapsed is 0"
  [& [fudge]]
  (+ (.getTime (Date.)) (or fudge 0)))

(defn producer [producer-num cnt]
  (rabbit/with-amqp
   {}
   (let [start-time (now -1)]
     (dotimes [ii cnt]
       (object-publish [ii (Date.)]))
     (log-producer-stat producer-num cnt start-time (now))
     (prn (format "producer-%s: %s published in %s @ %s/s"
                  producer-num
                  cnt
                  (/ (- (now) start-time) 1000.0)
                  (/ cnt (/ (- (now) start-time) 1000.0)))))))

(defn consumer [consumer-num]
  (rabbit/with-amqp
   {}
   (let [start-time (now -1)
         num-msgs   (atom 0)
         msg-age    (atom 0)]
     (loop [[ii dt] (rabbit/object-get)]
       (if ii
         (let [end         (now -1)
               msg-elapsed (- end (.getTime dt))]
           (reset! num-msgs (+ 1 @num-msgs))
           (reset! msg-age  (+ msg-elapsed @msg-age))
           (recur (rabbit/object-get)))))
     (log-consumer-stat consumer-num @num-msgs start-time (now) @msg-age)
     (let [elapsed (/ (- (now) start-time) 1000.0)
           rate    (/ @num-msgs elapsed)]
       (prn (format "consumer-%s: %s messages in %s elapsed seconds @ %s/second"
                    consumer-num
                    @num-msgs
                    elapsed
                    rate))))))

;; (com.github.kyleburton.sandbox.rabbitmq.perf-test/producer "test" 5000)
;; (com.github.kyleburton.sandbox.rabbitmq.perf-test/consumer "test-consumer")


(defn clear-stats []
  (.delete (java.io.File. *consumer-stats-file*))
  (.delete (java.io.File. *producer-stats-file*))
  (ensure-consumer-stats-file)
  (ensure-producer-stats-file))

;; TODO: need to have the consumer await the first message and not
;; just exit, then we wont' need the (Thread/sleep 100)
(defn run-single-benchmark [series broker num-runs num-msgs num-producers]
  (dotimes [run-number num-runs]
    ;; TODO: use agents to fire off the producers and consumers...
    (binding [*testing-info* (merge *testing-info*
                                    {:series  series
                                     :broker  broker
                                     :run-num (+ 1 run-number)})]
      (let [msgs-per-producer (/ num-msgs num-producers)]
        (dotimes [ii num-producers]
          (.start (Thread. (fn [] (producer (format "p%s" ii) msgs-per-producer))))))
      (Thread/sleep 100)
      ;; TODO: how to run multiple consumers in process?
      (consumer "c1"))))

;; (clear-stats)
;; (run-single-benchmark "Series1" "RabbitMQ" 3 100 1)

(defn run-benchmark [broker num-prods num-cons]
  (doseq [msg-count [1 5 10 50 100 500 1000 5000 10000 50000 100000]]
    (run-single-benchmark (format "Series-%sm-%sp-%sc" msg-count num-prods num-cons)
                          broker
                          3 
                          msg-count
                          num-prods)))

;; (clear-stats)
;; (run-benchmark "RabbitMQ"    1 1)
;; (run-benchmark "Apache-Qpid" 1 1)

(defn load-stats-file [fname]
  (with-open [f (ds/reader fname)]
    (binding [*in* f]
      (let [hdr (seq (.split (read-line) "\t"))]
        (loop [line (read-line)
               res []]
          (if line
            (do
              (recur (read-line) (conj res (zipmap hdr (seq (.split line "\t"))))))
            res))))))

(defn get-stat-data [broker file]
  (filter #(= (% "BROKER") broker)
          (load-stats-file *consumer-stats-file*)))

(defn get-xy-data [broker xname yname]
  (let [stat-data (get-stat-data broker *consumer-stats-file*)
        count-and-rate (map (fn [ent] [(ent xname) (ent yname)]) stat-data)
        x-vals    (map #(Double/parseDouble (% 0))  count-and-rate)
        y-vals    (map #(Double/parseDouble (% 1))  count-and-rate)]
    [x-vals y-vals]))

(defn simple-xy-plot [broker xname yname]
  (let [rabbit-data (get-xy-data broker xname yname)]
    (line-plot (rabbit-data 0) (rabbit-data 1)
               :title (format "%s vs %s" xname yname)
               :x-label xname
               :y-label yname)))

'(view (simple-xy-plot "RabbitMQ"    "TOTAL-MESSAGES" "M/S"))
'(view (simple-xy-plot "Apache-Qpid" "TOTAL-MESSAGES" "M/S"))

'(let [plot        (simple-xy-plot "RabbitMQ"    "TOTAL-MESSAGES" "M/S")
      qpid-data   (get-xy-data    "Apache-Qpid" "TOTAL-MESSAGES" "M/S")]
  (add-lines plot (qpid-data 0) (qpid-data 1))
  (view plot))
