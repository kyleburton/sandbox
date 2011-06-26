(ns rabbit-client.core
  (:import
   [com.rabbitmq.client
    ConnectionFactory
    Connection
    Channel
    Consumer
    MessageProperties
    Envelope
    AMQP$BasicProperties
    ShutdownSignalException])
  (:require
   [clj-etl-utils.log :as log]
   [rn.clorine.pool :as pool]
   [clojure.contrib.json :as json])
  (:use
   [clj-etl-utils.lang-utils :only [raise aprog1]]))



;; connections are managed in an atom containin a map along with the
;; connection information and credentials

;; use a record / protocol?
;; (defrecord AMQPConnection [host port user pass vhost exchange])


(defn ensure-connection! [conn]
  (when-not (:channel @conn)
    (let [factory (aprog1
                      (ConnectionFactory.)
                    (.setUsername it    (:user  @conn "guest"))
                    (.setPassword it    (:pass  @conn "guest"))
                    (.setVirtualHost it (:vhost @conn "/"))
                    (.setHost        it (:host  @conn "localhost"))
                    (.setPort        it (:port  @conn 5672)))
          connection (.newConnection factory)
          channel    (.createChannel connection)]
      (swap! conn assoc
             :factory factory
             :connection connection
             :channel    channel)))
  conn)

(defn ensure-connections! [conn]
  (doseq [conn (:connections conn)]
    (ensure-connection! conn))
  conn)

(defn close-quietly [thing]
  (let [result (atom {:close-result nil
                      :exception nil})]
    (try
     (swap! result assoc :close-result (.close thing))
     (catch Exception ex
       (swap! result assoc :exception ex)))
    @result))

(defn close-connection! [conn]
  (close-quietly (:connection @conn))
  (close-quietly (:channel @conn))
  (swap! conn dissoc :channel :connection :factory)
  conn)

(defn close-connections! [conn]
  (doseq [conn (:connections conn)]
    (close-connection! conn))
  conn)

(comment

  (def *foo* (ensure-connections! {:connections [(atom {:port 25671})
                                                 (atom {:port 25672})]}))

  (close-connections! *foo*)

  )

(defn publish [^Atom conn ^String exchange ^String routing-key ^Boolean mandatory ^Boolean immediate ^BasicProperties props ^bytes body retries & [ex]]
  (if (< retries 1)
    (raise (RuntimeException. "Error: exceeded max retries for publish." ex)))
  (ensure-connections! conn)
  ;; try publishing to all, ensure we publish to at least 1
  (let [num-published (atom 0)]
    (doseq [conn (:connections conn)]
      (try
       (.basicPublish (:channel @conn)
                      exchange
                      routing-key
                      true
                      (or immediate false)
                      MessageProperties/PERSISTENT_TEXT_PLAIN
                      body)
       (swap! num-published inc)
       (catch Exception ex
         (close-connection! conn))))
    (if (zero? @num-published)
      (publish conn exchange routing-key mandatory immediate props body (dec retries)))))

(defn make-consumer [handlers]
  (let [default-handler (fn [this consumer-tag]
                          nil)
        handlers (merge {:cancel default-handler
                         :consume default-handler
                         :recover default-handler
                         :shutdown default-handler}
                        handlers)
        {cancel :cancel
         consume :consume
         delivery :delivery
         recover :recover
         shutdown :shutdown} handlers
        conn (atom {})
        consumer (reify
                  Consumer
                  (^void handleCancelOk [^Consumer this ^String consumer-tag]
                         (cancel conn this consumer-tag))
                  (^void handleConsumeOk [^Consumer this ^String consumer-tag]
                         (consume conn this consumer-tag))
                  (^void handleDelivery [^Consumer this ^String consumer-tag ^Envelope envelope ^AMQP$BasicProperties properties ^bytes body]
                         (delivery conn this consumer-tag envelope properties body))
                  (^void handleShutdownSignal [^Consumer this ^String consumer-tag ^ShutdownSignalException sig]
                         (shutdown conn this consumer-tag sig))
                  (^void handleRecoverOk [^Consumer this]
                         (recover conn this)))]))



(defn shutdown-consumer! [consumer]
  (raise "implement this"))

(defn start-consumer! [consumer]
  (raise "implement this"))

(comment

  (def *c1* (make-consumer
             {:delivery (fn [conn consumer consumer-tag envelope properties body]
                          (.basicAck (:channel conn)))
              }))


)