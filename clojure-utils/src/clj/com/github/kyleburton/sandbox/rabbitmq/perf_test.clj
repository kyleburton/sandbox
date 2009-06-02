(ns com.github.kyleburton.sandbox.rabbitmq.perf-test
  (:use [com.github.kyleburton.sandbox.rabbitmq :as rabbit]))

(defn producer [cnt]
  (rabbit/with-amqp
   {}
   (dotimes [ii cnt]
     (object-publish [ii (java.util.Date.)]))))

(defn consumer []
  (rabbit/with-amqp
   {}
   (let [start-time (java.util.Date.)
         num-msgs   (atom 0)]
     (loop [[ii dt] (rabbit/object-get)]
       (if ii
         (let [now (java.util.Date.)
               elapsed (- (.getTime now) (.getTime dt))]
           (reset! num-msgs (+ 1 @num-msgs))
           ;;(prn (format "consume: %s ms ii=%s dt=%s" elapsed ii dt))
           (recur (rabbit/object-get)))))

     (let [elapsed (/ (- (.getTime (java.util.Date.))
                         (.getTime start-time))
                      1000.0)
           rate (/ @num-msgs elapsed)]
       (prn (format "%s messages in %s elapsed seconds @ %s/second"
                    @num-msgs
                    elapsed
                    rate))))))

(do (.start (Thread. (fn [] (producer 2500))))
    (.start (Thread. (fn [] (producer 2500))))
    (.start (Thread. (fn [] (producer 2500))))
    (.start (Thread. (fn [] (producer 2500))))
    (Thread/sleep 100)
    (consumer))


