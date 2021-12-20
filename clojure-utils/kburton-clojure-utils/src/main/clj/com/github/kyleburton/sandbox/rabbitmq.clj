(ns com.github.kyleburton.sandbox.rabbitmq
  (:import (com.rabbitmq.client AMQP)
           (com.rabbitmq.client Channel Connection ConnectionFactory
                                ConnectionParameters QueueingConsumer
                                RpcServer RpcClient))
  (:use [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]))


(def DELIVERY_MODE_NON_PERSISTENT 1)
(def DELIVERY_MODE_PERSISTENT     2)

(defn getprop [prop & [default]]
  (if-let [v (System/getProperty prop)]
    v
    default))

(defn rprop [prop & [default]]
  (getprop (str "com.github.kyleburton.sandbox.rabbitmq" prop)
           default))

(defn rprop->int [prop & [default]]
  (if-let [v (System/getProperty prop)]
    (Integer/parseInt v)
    default))

(def *amqp-port*         (rprop->int "port" com.rabbitmq.client.AMQP$PROTOCOL/PORT))
(def *amqp-host*         (rprop      "broker-hostname"        "localhost"))
(def *amqp-exchange*     (rprop      "default.exchange.name"  "com.github.kyleburton.sandbox.rabbitmq.default.exchange.name"))
(def *amqp-queue*        (rprop      "default.queue.name"     "com.github.kyleburton.sandbox.rabbitmq.default.queue.name"))
(def *amqp-rpc-exchange* (rprop      "default.rpc.queue.name" "com.github.kyleburton.sandbox.rabbitmq.default.rpc.queue.name"))
(def *amqp-rpc-queue*    (rprop      "default.rpc.queue.name" "com.github.kyleburton.sandbox.rabbitmq.default.rpc.queue.name"))
(def *amqp-timeout*      (rprop->int  "timeout" 250))

(def *amqp-message-properties*
     (let [prop (com.rabbitmq.client.AMQP$BasicProperties.)]
       (set! (.deliveryMode prop) DELIVERY_MODE_NON_PERSISTENT)
       prop))

(def *connection-params*
     (doto (ConnectionParameters.)
       (.setUsername "guest")
       (.setPassword "guest")))



;; (map #(.get % *amqp-message-properties*) (kutils/fields-seq *amqp-message-properties*))
;; (kutils/fields-and-values-map *amqp-message-properties*)

;; (def *rabbit-host-name* "localhost")
;; (def *amqp-routing-key* "my-routing-key")
;; (def *amqp-exchange-name* "my-exchange")
;; <field name="delivery mode" type="octet"> Non-persistent (1) or persistent (2) </field>

(def #^{:doc "Var for holding a default factory"}
     *amqp-factory* 
     (ConnectionFactory. *connection-params*))

(def #^{:doc "Var used to hold the connection to the factory."} 
     *factory* nil)

(def #^{:doc "Var used to hold the open connection on the broker."} 
     *connection* nil)

(def #^{:doc "Var used to hold the open channel on the connection."} 
     *channel* nil)

;; TODO: document more of the options and parameters that have meaning
;; in the *env*
(def #^{:doc "Var used to hold the current 'environment' for connections."} 
     *env* {:acknowledge true
            :host *amqp-host*
            :port *amqp-port*
            :user nil
            :pass nil
            })

(defn make-connection-params
  "Simplified constructor for ConnectionParameters."
  [props]
  (let [params (ConnectionParameters.)]
    (if-let [val (:user props)]      (.setUsername params val))
    (if-let [val (:username props)]  (.setUsername params val))
    (if-let [val (:user-name props)] (.setUsername params val))
    (if-let [val (:pass props)]      (.setPassword params val))
    (if-let [val (:password props)]  (.setPassword params val))
;;     (if (:pass props)         (.setPassword             params (:pass        props)))
;;     (if (:password props)     (.setPassword             params (:password    props)))
    (if (:vhost props)        (.setVirtualHost          params (:vhost       props)))
    (if (:channel-max props)  (.setRequestedChannelMax  params (:channel-max props)))
    (if (:frame-max props)    (.setRequestedFrame       params (:frame-max   props)))
    (if (:heartbeat props)    (.setRequestedHeartbeat   params (:heartbeat   props)))
    (if (:heart-beat props)   (.setRequestedHeartbeat   params (:heart-beat  props)))
    params))

;; (bean (make-connection-params {:user "guest" :pass "guest"}))

;; set these if you want to override...
(def *amqp-param-map* (atom
                          {:user "guest"
                           :pass "guest"}))


(defmacro with-amqp [params & body]
  `(do-amqp ~params (fn [] ~@body)))

;; params must be a hash...
;; TODO: support `:noclose' to prevent the channel from being automatically closed...
(defn do-amqp
  "Functional equivalent of with-amqp macro"
  [params fn]
  (binding [*env* (merge *env* @*amqp-param-map* params)]
     (binding [*factory*  (ConnectionFactory. (make-connection-params *env*))]
       (with-open [connection (.newConnection *factory* 
                                              (:host *env* *amqp-host*)
                                              (:port *env* *amqp-port*))]
         (binding [*connection* connection]
           (with-open [channel (.createChannel *connection*)]
             (binding [*channel* channel]
               ;(println (format "do-amqp: declaring exchange: %s" (:exchange *env* *amqp-exchange*)))
               (.exchangeDeclare *channel*
                                 (:exchange *env* *amqp-exchange*)
                                 (:exchange-type *env* "direct")
                                 (:exchange-durable *env* false))
               ;(println (format "do-amqp: declaring queue: %s" (:queue *env* *amqp-queue*)))
               (.queueDeclare *channel* (:queue *env* *amqp-queue*) (:queue-durable *env* false))
               (when (:binding-key *env* *amqp-queue*)
;;                  (println (format "do-amqp: binding queue:%s exchange:%s binding-key:%s" 
;;                                 (:queue *env* *amqp-queue*)
;;                                 (:exchange *env* *amqp-exchange*)
;;                                 (:binding-key *env* *amqp-queue*)))
                 (.queueBind *channel*
                             (:queue *env* *amqp-queue*)
                             (:exchange *env* *amqp-exchange*)
                             (:binding-key *env* *amqp-queue*)))
               ;(println (format "do-amqp: about to invoke fn/0"))
               (fn))))))))

;; (with-amqp
;;     [:exchange "test.exchange.name" 
;;      :exchange-durable false
;;      :queue "test.queue.name" :queue-durable false]
;;   (prn "broker=" *factory*)
;;   (prn "connection=" *connection*)
;;   (prn "env=" *env*))

;; (do-amqp
;;  {:exchange "test.exchange.name" 
;;   :exchange-durable false
;;   :queue "test.queue.name" :queue-durable false}
;;  (fn []
;;    (prn "broker=" *factory*)
;;    (prn "connection=" *connection*)
;;    (prn "env=" *env*)))

(defn basic-publish [#^java.util.Map params #^String message]
  (with-amqp 
      params
    (println (format "basic-publish: routing-key: %s" (:routing-key *env* *amqp-queue*)))
    (.basicPublish 
     *channel*
     (:exchange    *env* *amqp-exchange*)
     (:routing-key *env* *amqp-queue*)
     (:mandatory   *env* false)
     (:immediate   *env* false)
     nil ;; (:message-properties *env* *amqp-message-properties*)
     (.getBytes message))))

(defn object-publish
  "Publish an object, serializing it before publishing."
  [#^Object message]
  (.basicPublish 
   *channel*
   (:exchange    *env* *amqp-exchange*)
   (:routing-key *env* *amqp-queue*)
   (:mandatory   *env* false)
   (:immediate   *env* false)
   nil
   (kutils/freeze message)))

(defn basic-object-publish
  "Publish an object, serializing it before publishing."
  [#^java.util.Map params #^Object message]
  (with-amqp 
      params
    (object-publish message)))


;; (basic-publish {} (format "[%s] this is my message..." (java.util.Date.)))
;; (String. (.getBody (basic-get)))
;; (String. (.getBody (basic-get {:queue "SimpleQueue"})))

(defn basic-get
  "Immediate pull, even if there are no messages waiting."
  [& [#^java.util.Map params]]
  (with-amqp
      params
    ;;(prn (format "calling .basicGet on *channel*=%s" *channel*))
    (.basicGet *channel* 
               (:queue *env* *amqp-queue*) 
               (:acknowledge *env* true))))

(defn basic-object-get
  "Immediate pull, even if there are no messages waiting, assume the
  message body was serialized and deserialize it."  [&
  [#^java.util.Map params]]
  (let [msg (basic-get params)]
    (if (not (nil? msg))
      (kutils/thaw (.getBody msg))
      msg)))

(defn object-get
  "Immediate pull, even if there are no messages waiting, assume the
  message body was serialized and deserialize it."  
  []
  (let [msg (.basicGet *channel* 
                       (:queue *env* *amqp-queue*) 
                       (:acknowledge *env* true))]
    (if (not (nil? msg))
      (kutils/thaw (.getBody msg))
      msg)))

;; (object-publish {} (java.util.Date.))
;; (object-get)

(defn do-consume
  "Create a consumer and pass it to the given function."
  [params f]
  (do-amqp 
   params
   (fn []
     ;(println (format "do-consume: creating consumer..."))
     (let [consumer (let [consumer (QueueingConsumer. *channel*)]
                      (.basicConsume *channel* (:routing-key *env* *amqp-queue*) consumer)
                      consumer)]
       ;(println (format "do-consume: consumer=%s, about to invoke fn/1" consumer))
       (f consumer)))))

(defn ack-delivery
  "Manually acknowledge a delivery (message)."
  [consumer delivery]
  (.basicAck (.getChannel consumer)
             (.getDeliveryTag (.getEnvelope delivery))
             true))

;; (basic-publish {} (format "[%s] this is my message..." (java.util.Date.)))


(defn try-get
  "Using QueueingConsumer, try to pull off a message, returning nil if none were ready."
  [& [#^java.util.Map params]]
  (do-consume
   params
   (fn [consumer]
     (if-let [delivery (.nextDelivery consumer (:timeout *env* *amqp-timeout*))]
       (do
         (if (:acknowledge *env* true)
           (ack-delivery consumer delivery))
         (String. (.getBody delivery)))))))

;; (basic-publish {} (format "[%s] this is my message..." (java.util.Date.)))
;; (try-get {:acknowledge false})
;; (try-get)



;; (with-amqp [] (.queueDelete *channel* "SimpleQueue"))
;; (with-amqp [] (.queueDelete *channel* "test.queue.name"))
;; (with-amqp [] (.exchangeDelete *channel* "test.exchange.name"))

;; (with-amqp [] (.queueDelete *channel* *amqp-queue*))
;; (with-amqp [] (.exchangeDelete *channel* *amqp-exchange*))

;; (def x (make-rpc-state))
;; (:factory x)
;; (:channel x)
;; (:connection x)
;; (shutdown-rpc-state x)

(defn make-rpc-server [params callbacks]
  (binding [*env* (merge {:queue *amqp-rpc-queue*} *env* params)]
    (let [factory    (ConnectionFactory. (make-connection-params *env*))
          connection (.newConnection factory
                                     (:host *env* *amqp-host*) 
                                     (:port *env* *amqp-port*))
          channel    (.createChannel connection)
          rpc-state {:factory factory
                     :connection connection
                     :channel channel
                     :env *env* }
          queue    (:queue (:env rpc-state))
          exchange (:exchange *env* *amqp-rpc-exchange*)
          map-call (:map-call callbacks)
          map-cast (:map-cast callbacks)]
      (.exchangeDeclare channel exchange "direct" true)
      (.queueDeclare channel queue true)
      (.queueBind channel
                  queue
                  exchange
                  queue
                  )
      (prn (format "make-rpc-server: channel:%s queue:%s map-call:%s map-cast:%s"
                   channel
                   queue
                   (:map-call callbacks)
                   (:map-cast callbacks)))
      (let [server (proxy [com.rabbitmq.client.MapRpcServer]
                       [channel queue]
                     (handleMapCall [request props]
                                    ;;(prn (format "handleMapCall: this=%s request=%s props=%s" this request props))
                                    (map-call rpc-state this request props))
                     (handleMapCast [request]
                                    ;;(prn (format "handleMapCast: this=%s request=%s=%s" this request))
                                    (map-cast rpc-state this request)))]
        (assoc rpc-state :server server)))))

(defn shutdown-rpc-server [state]
  (try 
   (.close (:channel state))
   (catch Exception ex
     (prn "channel close failed: " ex)
     (throw ex))
   (finally
    (try 
     (.close (:connection state))
     (catch Exception ex
       (prn "connection close failed: " ex)
       (throw ex))     
     (finally
      (try
       (.close (:server state))
       (catch Exception ex
         (prn "server close failed: " ex)
         (throw ex))))))))

(defn map-call-handler [rpc-state this request props]
  (prn (format "map-call-handler: this=%s command=%s %s"
               this
               (.get request "command")
               (if (.get request "command")
                 (.toString (.get request "command"))
                 "")))
  (if (and (.get request "command")
           (= "exit" (.toString (.get request "command"))))
    (.terminateMainloop this))
  {"resp" (str "you pinged: " (if (.get request "ping")
                                (.toString (.get request "ping"))
                                "*nothing*"))
   "time" (java.util.Date.)})



'(

(def rpc-server
     (make-rpc-server 
      {};;{:queue "rpc.test"}
      {:map-call map-call-handler
       :map-cast (fn rpc-map-cast [rpc-state this request]
                   (prn (format "in the map-cast handler! rpc-state:%s this:%s request:%s" 
                                rpc-state this request)))}))

(:exchange (:env rpc-server) *amqp-rpc-exchange*)

;'(with-amqp [] (.queueDelete *channel* "com.github.kyleburton.sandbox.rabbitmq.default.rpc.queue.name"))

(do (.mainloop (:server rpc-server)) 
    (prn "mainloop returned")
    (try (shutdown-rpc-server rpc-server)
         (catch Exception ex
           (prn "error shutting down: " ex)
           (throw ex))))

(.getQueueName (:server rpc-server))

(defn close-rpc-client [client]
  (.close (.getChannel client))
  (.close client))

(def rpc-client
     (RpcClient. 
      (.createChannel 
       (.newConnection (:factory rpc-server)
                       (:host *env* *amqp-host*) 
                       (:port *env* *amqp-port*)))
      (:exchange (:env rpc-server) *amqp-rpc-exchange*)
      (:queue (:env rpc-server))))

(.getExchange rpc-client)

(let [res (.mapCall rpc-client {"ping" "value2"})]
  (prn (format "returned: %s" res)))

(let [res (.mapCall rpc-client {"command" "exit"})]
  (prn (format "returned: %s" res)))

(close-rpc-client rpc-client)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def *rpc-server-channel-key* "rpc.channel.key")
;; (def *rpc-server-connection* (.newConnection *amqp-factory* *amqp-host* *amqp-port*))
;; (def *rpc-server-channel*
;;      (let [channel (.createChannel *rpc-server-connection*)]
;;        (.exchangeDeclare channel "rpc.exchange" "direct" true)
;;        (.queueDeclare channel *rpc-server-channel-key* true)
;;        channel))

;; (def *rpc-server*
;;      (let [server (proxy [com.rabbitmq.client.MapRpcServer]
;;                       [*rpc-server-channel* *rpc-server-channel-key*]
;;                     (handleMapCall [request props]
;;                                    (prn "handle-map-call! req=" request)
;;                                    (if (.get request "exit")
;;                                      (do
;;                                        (prn "exiting...")
;;                                        (.terminateMainloop this)
;;                                        (.close this)))
;;                                    {"resp" "I hunger!"}))]
;;        (.queueDeclare (.getChannel server) *rpc-server-channel-key*)
;;        server))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment

;; (consume-message)

;; play w/the rpc client/server

(def *rpc-server-channel-key* "rpc-test")
(def *rpc-server-connection* (.newConnection *factory* *rabbit-host-name* *rabbit-port*))
(def *rpc-server-channel*    (open-channel *rpc-server-connection* *amqp-exchange-name* *rpc-server-channel-key*))
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

(def *rpc-client-channel*    (open-channel *rpc-client-connection* *amqp-exchange-name* *rpc-server-channel-key*))

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

)


