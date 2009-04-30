(ns rabbit-test
  (import (com.rabbitmq.client AMQP))
  (import (com.rabbitmq.client Channel Connection ConnectionFactory
           ConnectionParameters QueueingConsumer RpcServer RpcClient)))

(def *connection-params*
     (doto (ConnectionParameters.)
       (.setUsername "guest")
       (.setPassword "guest")))

(def *factory* (ConnectionFactory. *connection-params*))

(def *rabbit-port* com.rabbitmq.client.AMQP$PROTOCOL/PORT)

(def *rabbit-host-name* "localhost")

(def *connection* (.newConnection *factory* *rabbit-host-name* *rabbit-port*))

(def *client-connection* (.newConnection *factory* *rabbit-host-name* *rabbit-port*))

(def *routing-key* "my-routing-key")

(def *exchange-name* "my-exchange")

(defmacro with-connection [var & body]
  `(let [~var (.newConnection *factory* *rabbit-host-name* *rabbit-port*)]
     ~@body))

(def *msg-parms* 
     (let [prop (com.rabbitmq.client.AMQP$BasicProperties.)]
       (set! (.deliveryMode prop) 2)
       prop))

(defn declare-exchange-and-queue [#^Channel channel exchange-name queue-name]
      (.exchangeDeclare channel exchange-name "direct" true)
      (.queueDeclare channel queue-name true)
      channel)

(defn open-channel [#^Connection conn exchange-name queue-name]
  (let [ch (.createChannel conn)]
    (declare-exchange-and-queue ch exchange-name queue-name)))

(defn publish-message [message]
  (with-connection conn
    (let [channel (open-channel conn *exchange-name* *routing-key*)]
      (.basicPublish channel "" *routing-key* true true *msg-parms* (.getBytes (str message))))))

;; (publish-message "test message 2")

(defn consume-message []
  (with-connection conn
    (let [channel (open-channel conn *exchange-name* *routing-key*)]
      (prn (format "declared routing key:%s" *routing-key*))
      (let [queueing-consumer (let [consumer (QueueingConsumer. channel)]
                                (.basicConsume channel *routing-key* consumer)
                                consumer)]
        (prn (format "queueing-consumer:%s" queueing-consumer))
        (prn (format "queueing-consumer: queue.size=%s" (.size (.getQueue queueing-consumer))))
        (let [delivery (.nextDelivery queueing-consumer)]
          (prn (format "queueing-consumer:%s delivery:%s" queueing-consumer delivery))
          (if delivery
            (prn (format "consumed: message=%s" (String. (.getBody delivery))))))))))
;; (consume-message)

(def *queueing-consumer*
    (let [channel (open-channel *client-connection* *exchange-name* *routing-key*)]
      (.queueDeclare channel *routing-key* true)
      (let [consumer (QueueingConsumer. channel)]
        (.basicConsume channel *routing-key* consumer)
        consumer)))


'(

  (publish-message "test message 2")

  (let [delivery (.nextDelivery *queueing-consumer* 500)]
    (if delivery
      (let [msg (String. (.getBody delivery))]
        (.basicAck (.getChannel *queueing-consumer*)
                   (.getDeliveryTag (.getEnvelope delivery))
                   true)
        (prn (format "delivery=%s => %s" delivery msg)))))
  

  (.close (.getChannel *queueing-consumer*))


  )

'(
  (def x
       (let [channel (open-channel *client-connection* *exchange-name* *routing-key*)]
         (.basicGet channel *routing-key* false)))

  (String. (.getBody x))

  (let [channel (open-channel *client-connection* *exchange-name* *routing-key*)]
    (.basicAck channel
               (.getDeliveryTag (.getEnvelope x))
               true))

  (let [channel (open-channel *client-connection* *exchange-name* *routing-key*)]
    (seq (.getKnownHosts (.getConnection channel))))


  
)


;; play w/the rpc client/server

(def *rpc-server-channel-key* "rpc-test")

(def *rpc-server-connection* (.newConnection *factory* *rabbit-host-name* *rabbit-port*))

(def *rpc-server-channel*    (open-channel *rpc-server-connection* *exchange-name* *rpc-server-channel-key*))

(def *rpc-server*
     (let [server (proxy [com.rabbitmq.client.MapRpcServer]
                      [*rpc-server-channel* *rpc-server-channel-key*]
                    (handleMapCall [request props]
                                   (prn "handle-map-call! req=" request)
                                   (if (.get request "exit")
                                     (do
                                       (prn "exiting...")
                                       (.terminateMainloop this)
                                       (.close this)))
                                   {"resp" "I hunger!"}))]
       (.queueDeclare (.getChannel server) *rpc-server-channel-key*)
       server))


(def *rpc-client-connection* (.newConnection *factory* *rabbit-host-name* *rabbit-port*))

(def *rpc-client-channel*    (open-channel *rpc-client-connection* *exchange-name* *rpc-server-channel-key*))

(def *rpc-client*
     (RpcClient. *rpc-client-channel* "" *rpc-server-channel-key*))

'(

  (do (.mainloop *rpc-server*) 
      (prn "mainloop returned"))

  ;; (.getQueueName *rpc-server*)
  ;; (.close *rpc-server*)

  (let [res (.mapCall *rpc-client* {"ping" "value2"})]
    (prn (format "returned: %s" res)))

  (let [res (.mapCall *rpc-client* {"exit" "value2"})]
    (prn (format "returned: %s" res)))

  ;; (.getReplyQueue *rpc-client*)
  ;; (.close *rpc-client*)
  

)


