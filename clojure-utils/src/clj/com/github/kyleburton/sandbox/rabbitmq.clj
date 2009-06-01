(ns com.github.kyleburton.sandbox.rabbitmq
  (:import (com.rabbitmq.client AMQP)
           (com.rabbitmq.client Channel Connection ConnectionFactory
                                ConnectionParameters QueueingConsumer
                                RpcServer RpcClient))
  (:use [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]))

(def *connection-params*
     (doto (ConnectionParameters.)
       (.setUsername "guest")
       (.setPassword "guest")))

(def DELIVERY_MODE_NON_PERSISTENT 1)
(def DELIVERY_MODE_PERSISTENT 2)

(def *default-port* com.rabbitmq.client.AMQP$PROTOCOL/PORT)
(def *default-host*         "localhost")
(def *default-exchange*     "com.github.kyleburton.sandbox.rabbitmq.default.exchange.name")
(def *default-queue*        "com.github.kyleburton.sandbox.rabbitmq.default.queue.name")
(def *default-rpc-exchange* "com.github.kyleburton.sandbox.rabbitmq.default.rpc.queue.name")
(def *default-rpc-queue*    "com.github.kyleburton.sandbox.rabbitmq.default.rpc.queue.name")
(def *default-timeout*      250)
(def *default-binding-key*  *default-queue*)
(def *default-message-properties*
     (let [prop (com.rabbitmq.client.AMQP$BasicProperties.)]
       (set! (.deliveryMode prop) DELIVERY_MODE_NON_PERSISTENT)
       prop))

;; (map #(.get % *default-message-properties*) (kutils/fields-seq *default-message-properties*))
;; (kutils/fields-and-values-map *default-message-properties*)

;; (def *rabbit-host-name* "localhost")
;; (def *default-routing-key* "my-routing-key")
;; (def *default-exchange-name* "my-exchange")
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
            :host *default-host*
            :port *default-port*
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

(bean (make-connection-params {:user "guest" :pass "guest"}))

;; set these if you want to override...
(def *default-param-map* (atom
                          {:user "guest"
                           :pass "guest"}))


;; TODO: refactor with-connection and do-connection, too much shared
;; behavior for them to be so cut&paste
(defmacro with-connection [params & body]
  (let [[positional named] (kutils/parse-paired-arglist params)]
    `(binding [*env* ~(merge *env* @*default-param-map* named)]
       (binding [*factory*  (ConnectionFactory. (make-connection-params *env*))]
         (with-open [conn# (.newConnection *factory* 
                                                (:host *env* *default-host*)
                                                (:port *env* *default-port*))]
           (binding [*connection* conn#]
             (with-open [channel# (.createChannel *connection*)]
               (binding [*channel* channel#]
                 (prn (format "with-connection: declaring exchange: %s" (:exchange *env* *default-exchange*)))
                 (.exchangeDeclare *channel*
                                   (:exchange *env* *default-exchange*)
                                   (:exchange-type *env* "direct")
                                   (:exchange-durable *env* false))
                 (prn (format "with-connection: declaring queue: %s" (:queue *env* *default-queue*)))
                 (.queueDeclare *channel* 
                                (:queue *env* *default-queue*)
                                (:queue-durable *env* false))
                 (.queueBind *channel*
                             (:queue *env* *default-queue*)
                             (:exchange *env* *default-exchange*)
                             (:binding-key *env* *default-binding-key*))
                 ~@body))))))))

;; params must be a hash...
(defn do-connection
  "Functional equivalent of with-connection macro"
  [params fn]
  (binding [*env* (merge *env* @*default-param-map* params)]
     (binding [*factory*  (ConnectionFactory. (make-connection-params *env*))]
       (with-open [connection (.newConnection *factory* 
                                              (:host *env* *default-host*)
                                              (:port *env* *default-port*))]
         (binding [*connection* connection]
           (with-open [channel (.createChannel *connection*)]
             (binding [*channel* channel]
               (prn (format "do-connection: declaring exchange: %s" (:exchange *env* *default-exchange*)))
               (.exchangeDeclare *channel*
                                 (:exchange *env* *default-exchange*)
                                 (:exchange-type *env* "direct")
                                 (:exchange-durable *env* false))
               (prn (format "do-connection: declaring queue: %s" (:queue *env* *default-queue*)))
               (.queueDeclare *channel* (:queue *env* *default-queue*) (:queue-durable *env* false))
               (.queueBind *channel*
                           (:queue *env* *default-queue*)
                           (:exchange *env* *default-exchange*)
                           (:binding-key *env* *default-binding-key*))
               (fn))))))))

;; (with-connection
;;     [:exchange "test.exchange.name" 
;;      :exchange-durable false
;;      :queue "test.queue.name" :queue-durable false]
;;   (prn "broker=" *factory*)
;;   (prn "connection=" *connection*)
;;   (prn "env=" *env*))

;; (do-connection
;;  {:exchange "test.exchange.name" 
;;   :exchange-durable false
;;   :queue "test.queue.name" :queue-durable false}
;;  (fn []
;;    (prn "broker=" *factory*)
;;    (prn "connection=" *connection*)
;;    (prn "env=" *env*)))

(defn basic-publish [#^java.util.Map params #^String message]
  (do-connection 
   params
   (fn []
     (.basicPublish 
      *channel*
      (:exchange    *env* *default-exchange*)
      (:routing-key *env* *default-queue*)
      (:mandatory   *env* false)
      (:immediate   *env* false)
      nil   ;; (:message-properties *env* *default-message-properties*)
      (.getBytes message)))))

;; (basic-publish {} (format "[%s] this is my message..." (java.util.Date.)))
;; (String. (.getBody (basic-get)))
;; (String. (.getBody (basic-get {:queue "SimpleQueue"})))

(defn basic-get
  "Immediate pull, even if there are no messages waiting."
  [& [#^java.util.Map params]]
  (do-connection
   params
   (fn []
     (prn (format "calling .basicGet on *channel*=%s" *channel*))
     (.basicGet *channel* 
                (:queue *env* *default-queue*) 
                (:acknowledge *env* true)))))

(defn do-consume [params f]
  (do-connection 
   params
   (fn []
     (let [consumer (let [consumer (QueueingConsumer. *channel*)]
                      (.basicConsume *channel* (:routing-key *env* *default-queue*) consumer)
                      consumer)]
       (f consumer)))))

(defn ack-delivery [consumer delivery]
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
     (if-let [delivery (.nextDelivery consumer (:timeout *env* *default-timeout*))]
       (do
         (if (:acknowledge *env* true)
           (ack-delivery consumer delivery))
         (String. (.getBody delivery)))))))

;; (basic-publish {} (format "[%s] this is my message..." (java.util.Date.)))
;; (try-get {:acknowledge false})
;; (try-get)



;; (with-connection [] (.queueDelete *channel* "SimpleQueue"))
;; (with-connection [] (.queueDelete *channel* "test.queue.name"))
;; (with-connection [] (.exchangeDelete *channel* "test.exchange.name"))

;; (with-connection [] (.queueDelete *channel* *default-queue*))
;; (with-connection [] (.exchangeDelete *channel* *default-exchange*))

;; (def x (make-rpc-state))
;; (:factory x)
;; (:channel x)
;; (:connection x)
;; (shutdown-rpc-state x)

(defn make-rpc-server [params callbacks]
  (binding [*env* (merge {:queue *default-rpc-queue*} *env* params)]
    (let [factory    (ConnectionFactory. (make-connection-params *env*))
          connection (.newConnection factory
                                     (:host *env* *default-host*) 
                                     (:port *env* *default-port*))
          channel    (.createChannel connection)
          rpc-state {:factory factory
                     :connection connection
                     :channel channel
                     :env *env* }
          queue    (:queue (:env rpc-state))
          exchange (:exchange *env* *default-rpc-exchange*)
          map-call (:map-call callbacks)
          map-cast (:map-cast callbacks)]
      (.exchangeDeclare channel exchange "direct" true)
      (.queueDeclare channel queue true)
      (.queueBind channel
                  queue
                  exchange
                  queue ;(:binding-key *env* *default-binding-key*)
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

(:exchange (:env rpc-server) *default-rpc-exchange*)

(with-connection [] (.queueDelete *channel* "com.github.kyleburton.sandbox.rabbitmq.default.rpc.queue.name"))

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
                       (:host *env* *default-host*) 
                       (:port *env* *default-port*)))
      (:exchange (:env rpc-server) *default-rpc-exchange*)
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
;; (def *rpc-server-connection* (.newConnection *amqp-factory* *default-host* *default-port*))
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
(def *rpc-server-channel*    (open-channel *rpc-server-connection* *default-exchange-name* *rpc-server-channel-key*))
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

(def *rpc-client-channel*    (open-channel *rpc-client-connection* *default-exchange-name* *rpc-server-channel-key*))

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


