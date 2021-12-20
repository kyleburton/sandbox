(ns rabbit-client.core
  (:import
   [java.io IOException]
   [com.rabbitmq.client
    ConnectionFactory
    Connection
    Channel
    Consumer
    AlreadyClosedException
    ReturnListener
    ConfirmListener
    MessageProperties
    Envelope
    AMQP$BasicProperties
    ShutdownSignalException]
   [com.github.kyleburton.teporingo BreakerOpenException])
  (:require
   [clj-etl-utils.log :as log]
   [rabbit-client.breaker :as breaker]
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

(def *default-routing-key* "#")

(defn ensure-connection! [conn]
  (if (contains? conn :connections)
    (doseq [conn (:connections conn)]
      (ensure-connection! conn))
    (when (nil? (:channel @conn))
      (let [factory (aprog1
                        (ConnectionFactory.)
                      (.setConnectionTimeout it (:connection-timeout @conn 0))
                      (.setUsername          it (:user  @conn "guest"))
                      (.setPassword          it (:pass  @conn "guest"))
                      (.setVirtualHost       it (:vhost @conn "/"))
                      (.setHost              it (:host  @conn "localhost"))
                      (.setPort              it (:port  @conn 5672)))
            connection (.newConnection factory)
            channel    (.createChannel connection)]
        (log/infof "ensure-connection!: %s" @conn)
        (when (:use-confirm @conn)
          (log/infof "using confirmSelect")
          (.confirmSelect channel))
        (when (:use-transactions @conn)
          (.txSelect channel))
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

(defn exchange-declare! [conn & [exchange-name exchange-type exchange-durable]]
  (if (contains? @conn :connections)
    (doseq [conn (:connections @conn)]
      (exchange-declare! conn exchange-name exchange-type exchange-durable))
    (.exchangeDeclare
     (:channel          @conn)
     (:exchange-name    @conn exchange-name)
     (:exchange-type    @conn (or exchange-type "direct"))
     (:exchange-durable @conn (or exchange-durable true)))))

(defn queue-declare! [conn & [name durable exclusive autodelete arguments]]
  (if (contains? @conn :connections)
    (doseq [conn (:connections @conn)]
      (queue-declare! conn name durable exclusive autodelete arguments))
    (.queueDeclare
     (:channel          @conn)
     (:queue-name       @conn name)
     (:queue-durable    @conn (if-not (nil? durable)    durable   true))
     (:queue-exclusive  @conn (if-not (nil? exclusive)  exclusive false))
     (:queue-autodelete @conn (if-not (nil? autodelete) autodelete false))
     (:queue-arguments  @conn (or arguments {})))))

(defn queue-bind! [conn & [queue-name exchange-name routing-key]]
  (if (contains? @conn :connections)
    (doseq [conn (:connections @conn)]
      (queue-bind! conn queue-name exchange-name routing-key))
    (.queueBind
     (:channel          @conn)
     (:queue-name       @conn (or queue-name    ""))
     (:exchange-name    @conn (or exchange-name ""))
     (:routing-key      @conn (or routing-key   *default-routing-key*)))))

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


(defn make-confirm-listener [conn handlers]
  (let [ack-fn  (:handle-ack handlers)
        nack-fn (:handle-nack handlers)
        confirm-listener
        (proxy
            [ConfirmListener]
            []
          (handleAck [delivery-tag multiple]
                     (ack-fn conn this delivery-tag multiple))
          (handleNack [delivery-tag multiple]
                      (nack-fn conn this delivery-tag multiple)))]
    {:conn     conn
     :type     :confirm-listener
     :listener confirm-listener}))


(defn attach-listener! [conn listener]
  (let [listener-type (:type listener)
        channel       (:channel @conn)
        listener      (:listener listener)]
    ;;(log/debugf "attaching listener: %s/%s to %s" listener-type listener @conn)
    (cond
      (= :consumer listener-type)
      (do
        (.basicConsume channel
                       (:queue-name   @conn)
                       (:auto-ack     @conn false)
                       (:consumer-tag @conn "")
                       listener))
      (= :return-listener listener-type)
      (.setReturnListener channel  listener)
      (= :confirm-listener listener-type)
      (.setConfirmListener channel listener)
      (= :default-consumer listener-type)
      (.setDefaultConsumer channel listener)
      (= :flow-listener     listener-type)
      (.setFlowListener    channel listener)
      :else
      (raise "Error: unrecognized listener type: %s (not one of: :consumer or :return-listener)" listener-type)))  )

(defn make-publish-circuit-breaker [opts]
  (breaker/basic-breaker
   (fn [conn exchange routing-key mandatory immediate props body]

     (try
      (ensure-publisher conn)
      (let [channel (:channel @conn)]
        (if channel
          (do
            (.basicPublish
             channel
             exchange
             routing-key
             mandatory
             immediate
             props
             body)
            (if (:use-transactions conn)
              (.txCommit channel))
            {:res true :ex nil})
          {:res false :ex nil}))
      (catch AlreadyClosedException ex
        (log/errorf ex "Error publishing to %s: %s" @conn ex)
        (close-connection! conn)
        (throw ex))
      (catch IOException ex
        (log/errorf ex "Error publishing to %s: %s" @conn ex)
        (close-connection! conn)
        (throw ex))))
   opts))

(defn ensure-publisher [conn]
  (if (contains? conn :connections)
    (doseq [conn (:connections conn)]
      (ensure-publisher conn))
    (when (nil? (:channel conn))
      (ensure-connection! conn)
      (exchange-declare!  conn)
      (queue-declare!     conn)
      (queue-bind!        conn)
      (swap! conn
             assoc
             :message-acks
             (java.util.concurrent.ConcurrentHashMap.))
      (attach-listener!
       conn
       (make-return-listener
        conn
        {:handle-return
         (fn [conn listener reply-code reply-text exchange routing-key props body]
           (let [msg (format "RETURNED: conn=%s code=%s text=%s exchange=%s routing-key:%s props=%s body=%s"
                             @conn
                             reply-code
                             reply-text
                             exchange
                             routing-key
                             props
                             (String. body))]
             (log/errorf msg)))}))
      (attach-listener!
       conn
       (make-confirm-listener
        conn
        {:handle-ack
         (fn [conn listener delivery-tag multiple]
           (log/warnf "handle-ack: %s %s %s" @conn delivery-tag multiple))
         :handle-nack
         (fn [conn listener delivery-tag multiple]
           (log/warnf "handle-nack: %s %s %s" @conn delivery-tag multiple))}))
      {:res true :ex nil}))
  conn)

;; NB: for performance, publish-1's use of ensure-publisher will have
;; to implement a circuit breaker - the timeout on establishing a
;; connection takes way too long for this to be a viable approach -
;; it'll end up creating too much back pressure in the event we've got
;; 1 or more brokers down.

;; NB: how do we handle when the message goes no where?
;; it's not a confirmListner, b/c in this event the message is returned
;; when it's returned it doesn't have a sequenceNo on the message, so
;; there is no way to coorelate the message we attempted to publish
;; with the one that was returned
(defn publish-1 [^Atom conn ^String exchange ^String routing-key ^Boolean mandatory ^Boolean immediate ^AMQP$BasicProperties props ^bytes body]
  (try
   ((:publish @conn) conn exchange routing-key mandatory immediate props body)
   (log/infof "publish-1: call to :publish succeeded")
   {:res true :ex nil}
   (catch IOException ex
     (log/errorf ex "Error: conn[%s] initilizing the publisher: %s" @conn ex)
     (close-connection! conn)
     {:res false :ex ex})
   (catch BreakerOpenException ex
     (log/errorf ex "Error: conn[%s] circuit breaker is open: %s" @conn ex)
     {:res false :ex ex})))

(defn publish [^Map publisher ^String exchange ^String routing-key ^Boolean mandatory ^Boolean immediate ^AMQP$BasicProperties props ^bytes body retries & [errors]]
  (when (< retries 1)
    (log/errorf "Error: exceeded max retries for publish %s : %s" publisher
                (vec errors))
    (doseq [err errors]
      (if err
        (log/errorf err "Max retries due to: %s" err)))
    (raise (RuntimeException. "Error: exceeded max retries for publish." (first errors))))
  ;; try publishing to all brokers, ensure we publish to at least the min required
  (let [num-published             (atom 0)
        min-brokers-published-to  (:min-brokers-published-to publisher 1)
        pub-errs                  (atom [])
        mandatory                 (if-not (nil? mandatory) mandatory true)
        immediate                 (if-not (nil? immediate) immediate true)
        message-props             (or props MessageProperties/PERSISTENT_TEXT_PLAIN)]
    (doseq [conn (:connections publisher)]
      (let [res (publish-1 conn exchange routing-key mandatory immediate props body)]
        (if (:res res)
          (swap! num-published inc)
          (swap! pub-errs conj (:ex res)))))
    (if (< @num-published min-brokers-published-to)
      (do
        (log/debugf "num-published %s was <%s, retrying..." @num-published min-brokers-published-to)
        (publish publisher exchange routing-key mandatory immediate props body (dec retries) (concat errors @pub-errs)))
      (log/debugf "looks like we published to %s brokers.\n" @num-published))))


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
  (shutdown-consumer-quietly! consumer)
  (doseq [listener (:listeners consumer)]
    (let [conn          (:conn listener)
          channel       (:channel   @conn)
          exchange-name (:exchange-name  @conn "")
          queue-name    (:queue-name @conn "")
          routing-key   (:routing-key @conn *default-routing-key*)
          listener-type (:type listener)]
      (ensure-connection! conn)
      (exchange-declare! conn)
      (queue-declare!    conn)
      (queue-bind!       conn)
      (attach-listener!  conn listener)))
  consumer)

(defn make-default-return-listener [conn]
  (make-return-listener
   conn
   {:handle-return
    (fn [conn listener reply-code reply-text exchange routing-key props body]
      (let [msg (format "RETURNED: conn=%s code=%s text=%s exchange=%s routing-key:%s props=%s body=%s"
                        @conn
                        reply-code
                        reply-text
                        exchange
                        routing-key
                        props
                        (String. body))]
        (log/errorf msg)))}))


(comment

  (def *c1*
       (let [conn (atom {:port 25672
                         :vhost           "/"
                         :exchange-name   "/foof"
                         :queue-name      "foofq"})]
         {:listeners
          [(make-consumer
            conn
            {:delivery
             (fn [conn consumer consumer-tag envelope properties body]
               (try
                (log/infof "CONSUMER: got a delivery")
                (let [msg (String. body)]
                  (log/infof "CONSUMER: body='%s'" msg))
                (.basicAck (:channel @conn)
                           (.getDeliveryTag envelope) ;; delivery tag
                           false)                      ;; multiple
                (catch Exception ex
                  (log/errorf ex "Consumer Error: %s" ex))))})
           (make-default-return-listener conn)]}))


  (start-consumer! *c1*)

  (shutdown-consumer-quietly! *c1*)

  (def *c2*
       (let [conn (atom {:port 25671
                         :vhost           "/"
                         :exchange-name   "/foof"
                         :queue-name      "foofq"})]
         {:listeners
          [(make-consumer
            conn
            {:delivery
             (fn [conn consumer consumer-tag envelope properties body]
               (try
                (log/infof "CONSUMER: got a delivery")
                (let [msg (String. body)]
                  (log/infof "CONSUMER: body='%s'" msg))
                (.basicAck (:channel @conn)
                           (.getDeliveryTag envelope) ;; delivery tag
                           false)                      ;; multiple
                (catch Exception ex
                  (log/errorf ex "Consumer Error: %s" ex))))})
           (make-default-return-listener conn)]}))

  (start-consumer! *c2*)

  (shutdown-consumer-quietly! *c2*)


  (do
    (def *foo*
         {:connections [(atom {:name "rabbit-1"
                               :port 25671
                               :use-confirm true
                               :connection-timeout 10
                               :queue-name "foofq"
                               :routing-key ""
                               :vhost "/"
                               :exchange-name "/foof"
                               :publish (make-publish-circuit-breaker
                                         {:retry-after 10})})
                        (atom {:name "rabbit-2"
                               :port 25672
                               :connecton-timeout 10
                               :use-confirm true
                               :queue-name "foofq"
                               :routing-key "#"
                               :vhost "/"
                               :exchange-name "/foof"
                               :publish (make-publish-circuit-breaker
                                         {:retry-after 10})})]}))

  (close-connection! *foo*)
  (.getConfirmListener (:channel @(first (:connections *foo*))))


  (.getSocketFactory (:factory @(first (:connections *foo*))))

  (.getConnectionTimeout (:factory @(first (:connections *foo*))))
  (.getConnectionTimeout (:factory @(second (:connections *foo*))))

  (dotimes [ii 100]
    (try
     (publish
      *foo*
      "/foof"
      ""
      true
      false
      MessageProperties/PERSISTENT_TEXT_PLAIN
      (.getBytes (str "hello there:" ii))
      2)
     (printf "SUCCESS[%s]: Published to at least 1 broker.\n" ii)
     (catch Exception ex
       (printf "FAILURE[%s] %s\n" ii ex))))



  )