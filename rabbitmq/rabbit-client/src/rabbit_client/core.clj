(ns rabbit-client.core
  (:import
   [java.io IOException]
   [com.rabbitmq.client
    ConnectionFactory
    Connection
    Channel
    Consumer
    ReturnListener
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

;; http://en.wikipedia.org/wiki/Volcano_Rabbit
;;   => teporingo

;; a 'connection' is a map: {:connections [...]}, where each
;; connection is a managed map wrapped in an atom where each map
;; contains the connection information and broker credentails, along
;; with the connection information and credentials


(defn ensure-connection! [conn]
  (if (contains? conn :connections)
    (doseq [conn (:connections conn)]
      (ensure-connection! conn))
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
               :channel    channel))))
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
  (if (contains? conn :connections)
    (doseq [conn (:connections conn)]
      (close-connection! conn))
    (do
      (close-quietly (:connection @conn))
      (close-quietly (:channel @conn))
      (swap! conn dissoc :channel :connection :factory)))
  conn)

(defn exchange-declare [conn & [exchange-name exchange-type exchange-durable]]
  (if (contains? @conn :connections)
    (doseq [conn (:connections @conn)]
      (exchange-declare conn exchange-name exchange-type exchange-durable))
    (.exchangeDeclare
     (:channel          @conn)
     (:exchange-name    @conn exchange-name)
     (:exchange-type    @conn (or exchange-type "direct"))
     (:exchange-durable @conn (or exchange-durable true)))))

(defn queue-declare [conn & [name durable exclusive autodelete arguments]]
  (if (contains? @conn :connections)
    (doseq [conn (:connections @conn)]
      (queue-declare conn name durable exclusive autodelete arguments))
    (.queueDeclare
     (:channel          @conn)
     (:queue-name       @conn name)
     (:queue-durable    @conn (if-not (nil? durable)    durable   true))
     (:queue-exclusive  @conn (if-not (nil? exclusive)  exclusive false))
     (:queue-autodelete @conn (if-not (nil? autodelete) autodelete false))
     (:queue-arguments  @conn (or arguments {})))))

(defn queue-bind [conn & [queue-name exchange-name routing-key]]
  (if (contains? @conn :connections)
    (doseq [conn (:connections @conn)]
      (queue-bind conn queue-name exchange-name routing-key))
    (.queueBind
     (:channel          @conn)
     (:queue-name       @conn (or queue-name    ""))
     (:exchange-name    @conn (or exchange-name ""))
     (:routing-key      @conn (or routing-key   "")))))

(defn publish-1 [^Atom conn ^String exchange ^String routing-key ^Boolean mandatory ^Boolean immediate ^AMQP$BasicProperties props ^bytes body]
  (try
   (printf "immediate=%s;mandatory=%s publishing to %s\n" immediate mandatory @conn)
   (.basicPublish
    (:channel @conn)
    exchange
    routing-key
    mandatory
    immediate
    props
    body)
   (printf "Publish Succeeded: %s/%s\n" exchange routing-key)
   {:res true :ex nil}
   (catch IOException ex
     (printf "Error publishing: %s\n" ex)
     (.printStackTrace ex)
     (log/warnf "Error during one of the publishes to: %s : %s\n" conn ex)
     (close-connection! conn)
     (printf "closed connection: %s\n" conn)
     {:res false :ex ex})))

(defn publish [^Map conn ^String exchange ^String routing-key ^Boolean mandatory ^Boolean immediate ^AMQP$BasicProperties props ^bytes body retries & [errors]]
  (when (< retries 1)
    (log/errorf "Error: exceeded max retries for publish %s" @conn)
    (doseq [err errors]
      (log/errorf err))
    (raise (RuntimeException. "Error: exceeded max retries for publish." (first errors))))
  (do
    (ensure-connection! conn)
    ;; try publishing to all, ensure we publish to at least 1
    (let [num-published  (atom 0)
          pub-errs       (atom [])
          mandatory      (or mandatory true)
          immediate      (or immediate true)
          message-props  (or props MessageProperties/PERSISTENT_TEXT_PLAIN)]
      (doseq [conn (:connections conn)]
        (let [res (publish-1 conn exchange routing-key mandatory immediate props body)]
          (if (:res res)
            (swap! num-published inc)
            (swap! pub-errs conj (:ex res)))))
      (if (zero? @num-published)
        (do
          (printf "num-published was %s, recursing...\n" @num-published)
          (publish conn exchange routing-key mandatory immediate props body (dec retries) (concat errors @pub-errs)))
        (printf "looks like we published to %s brokers." @num-published)))))


(defn make-consumer [conn handlers]
  (let [default-handler (fn [& args]
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
                         (recover conn this)))]
    {:conn conn
     :type :consumer
     :listener consumer}))

(defn make-return-listener [conn handlers]
  (let [handle-return-fn (:handle-return handlers)
        return-listener
        (proxy
            [ReturnListener]
            []
          (handleReturn [reply-code reply-text exchange routing-key props body]
                        (handle-return-fn conn this reply-code reply-text exchange routing-key props body)))]
    {:conn     conn
     :type     :return-listener
     :listener return-listener}))

(defn shutdown-consumer! [consumer]
  (doseq [listener (:listeners consumer)]
    (cond
      (= :consumer (:type listener))
      (.basicCancel
       (:channel @(:conn listener))
       (:consumer-tag @(:conn listener ""))))
    (close-connection! listener))
  consumer)

(defn shutdown-consumer-quietly! [consumer]
  (try
   (shutdown-consumer! consumer)
   (catch Exception ex
     nil))
  (doseq [listener (:listeners consumer)]
    (close-connection! (:conn listener)))
  consumer)

(defn start-consumer! [consumer]
  (doseq [listener (:listeners consumer)]
    (let [conn          (:conn listener)
          channel       (:channel   @conn)
          exchange-name (:exchange-name  @conn "")
          queue-name    (:queue-name @conn "")
          routing-key   (:routing-key @conn "")
          listener-type (:type listener)]
      (ensure-connection! conn)
      (exchange-declare conn)
      (queue-declare conn)
      (queue-bind conn)
      (cond
        (= :consumer listener-type)
        (do
          (printf "attaching consumer...\n")
          (.basicConsume (:channel      @conn)
                         (:queue-name   @conn)
                         (:auto-ack     @conn false)
                         (:consumer-tag @conn "")
                         (:listener     listener)))
        (= :return-listener listener-type)
        (do
          (printf "attaching return-listener...\n")
          (.setReturnListener (:channel @conn)
                              (:listener listener)))
        ;; TODO: support confirm, default and flow listener types
        :else
        (raise "Error: unrecognized listener type: %s (not one of: :consumer or :return-listener)" listener-type))))
  consumer)

(comment

  (def *c1*
       (let [conn (atom {:port 25672
                         :exchange-name   "test"
                         :queue-name "/"})]
         {:listeners
          [(make-consumer
            conn
            {:delivery (fn [conn consumer consumer-tag envelope properties body]
                         (let [msg (String. body)]
                           (println "got a delivery! %s\n" msg))
                         (.basicAck (:channel @conn)))})
           (make-return-listener
            conn
            {:handle-return
             (fn [conn listener reply-code reply-text exchange routing-key props body]
               (let [msg (format "RETURNED: code=%s text=%s exchange=%s routing-key:%s props=%s body=%s"
                                 reply-code
                                 reply-text
                                 exchange
                                 routing-key
                                 props
                                 (String. body))]
                 (println msg)
                 (log/errorf msg)))})]}))


  (start-consumer! *c1*)

  (shutdown-consumer-quietly! *c1*)

  (def *foo* {:connections [(atom {:port 25671})
                            (atom {:port 25672})]})
  (close-connection! *foo*)

  (do
    (publish
     *foo*
     "/foof"
     "/blarf"
     true
     true
     MessageProperties/PERSISTENT_TEXT_PLAIN
     (.getBytes "hello there")
     2))


  )