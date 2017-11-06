(ns mongo-start.mongo
  (:require
   [clojure.data.json      :as json]
   [clojure.string         :as string]
   [clojure.reflect        :as reflect]
   [camel-snake-kebab.core :as csk]
   [clojure.tools.logging  :as log])
  (:import
   [com.mongodb               MongoClient   MongoClientURI ServerAddress
    MongoClientOptions MongoClientOptions$Builder
    ServerAddress BasicDBObject DBCursor
    MongoCredential]
   [com.mongodb.client        MongoDatabase MongoCollection MongoCursor]
   [com.mongodb               Block]
   [com.mongodb.client.result DeleteResult UpdateResult]
   [org.bson.types ObjectId]))

;; static com.mongodb.client.model.Filters.*;
;; static com.mongodb.client.model.Updates.*;

;; TODO: look into and support authentiation
;; TODO: allow the databse & collection to be specified in connect-info?

;; connect-info can be any of
;; * string
;; * map of options
(defn connect-info->server-address [connect-info]
  (cond
    (contains? connect-info :host)
    [:singleton
     (ServerAddress. ^String (:host connect-info)
                     ^int (:port connect-info 27017))]

    (contains? connect-info :server-addresses)
    [:list
     (mapv
      (fn [[^String host ^int port]]
        (ServerAddress. host port))
      (:server-addresses connect-info))]
    
    :otherwise
    (throw (RuntimeException.
            (format "Error: don't know how to get server-addresses out of: %s : %s"
                    (class connect-info)
                    connect-info)))))

;; https://stackoverflow.com/questions/29030526/mongoclient-not-respecting-connecttimeout
;; MongoClientOptions.builder().serverSelectionTimeout(500).build()
(comment

  ;; NB: this generates code for the body make-client-options
  (let [setters (->>
                 (->
                  MongoClientOptions$Builder
                  reflect/reflect
                  :members)
                 (filterv (fn [member]
                            (and
                             (-> member :flags :public)
                             (= 1 (-> member :parameter-types count))))))]
    (->>
     setters
     (map
      (fn [setter]
        ;; symbol setting form
        (let [kwd       (-> setter :name name csk/->kebab-case keyword)
              setter-fn (symbol (str "." (:name setter)))
              type-hint (symbol (str "^" (-> setter :parameter-types first)))]
          `(~'when (~'contains? ~'connect-info ~kwd)
            (~setter-fn ~'builder ~type-hint (~kwd ~'connect-info))))))))
  
  )

(defn make-client-options ^MongoClientOptions [connect-info]
  (let [builder (com.mongodb.MongoClientOptions/builder)]
    (when (contains? connect-info :min-connections-per-host) (.minConnectionsPerHost builder ^int (:min-connections-per-host connect-info)))
    (when (contains? connect-info :always-use-m-beans) (.alwaysUseMBeans builder ^boolean (:always-use-m-beans connect-info)))
    (when (contains? connect-info :min-heartbeat-frequency) (.minHeartbeatFrequency builder ^int (:min-heartbeat-frequency connect-info)))
    (when (contains? connect-info :max-connection-idle-time) (.maxConnectionIdleTime builder ^int (:max-connection-idle-time connect-info)))
    (when (contains? connect-info :add-server-listener) (.addServerListener builder ^com.mongodb.event.ServerListener (:add-server-listener connect-info)))
    (when (contains? connect-info :server-selection-timeout) (.serverSelectionTimeout builder ^int (:server-selection-timeout connect-info)))
    (when (contains? connect-info :connect-timeout) (.connectTimeout builder ^int (:connect-timeout connect-info)))
    (when (contains? connect-info :heartbeat-socket-timeout) (.heartbeatSocketTimeout builder ^int (:heartbeat-socket-timeout connect-info)))
    (when (contains? connect-info :add-connection-pool-listener) (.addConnectionPoolListener builder ^com.mongodb.event.ConnectionPoolListener (:add-connection-pool-listener connect-info)))
    (when (contains? connect-info :add-cluster-listener) (.addClusterListener builder ^com.mongodb.event.ClusterListener (:add-cluster-listener connect-info)))
    (when (contains? connect-info :heartbeat-frequency) (.heartbeatFrequency builder ^int (:heartbeat-frequency connect-info)))
    (when (contains? connect-info :read-concern) (.readConcern builder ^com.mongodb.ReadConcern (:read-concern connect-info)))
    (when (contains? connect-info :cursor-finalizer-enabled) (.cursorFinalizerEnabled builder ^boolean (:cursor-finalizer-enabled connect-info)))
    (when (contains? connect-info :heartbeat-connect-timeout) (.heartbeatConnectTimeout builder ^int (:heartbeat-connect-timeout connect-info)))
    (when (contains? connect-info :codec-registry) (.codecRegistry builder ^org.bson.codecs.configuration.CodecRegistry (:codec-registry connect-info)))
    (when (contains? connect-info :connections-per-host) (.connectionsPerHost builder ^int (:connections-per-host connect-info)))
    (when (contains? connect-info :ssl-context) (.sslContext builder ^javax.net.ssl.SSLContext (:ssl-context connect-info)))
    (when (contains? connect-info :db-encoder-factory) (.dbEncoderFactory builder ^com.mongodb.DBEncoderFactory (:db-encoder-factory connect-info)))
    (when (contains? connect-info :ssl-enabled) (.sslEnabled builder ^boolean (:ssl-enabled connect-info)))
    (when (contains? connect-info :description) (.description builder ^java.lang.String (:description connect-info)))
    (when (contains? connect-info :socket-factory) (.socketFactory builder ^javax.net.SocketFactory (:socket-factory connect-info)))
    (when (contains? connect-info :local-threshold) (.localThreshold builder ^int (:local-threshold connect-info)))
    (when (contains? connect-info :socket-keep-alive) (.socketKeepAlive builder ^boolean (:socket-keep-alive connect-info)))
    (when (contains? connect-info :ssl-invalid-host-name-allowed) (.sslInvalidHostNameAllowed builder ^boolean (:ssl-invalid-host-name-allowed connect-info)))
    (when (contains? connect-info :required-replica-set-name) (.requiredReplicaSetName builder ^java.lang.String (:required-replica-set-name connect-info)))
    (when (contains? connect-info :db-decoder-factory) (.dbDecoderFactory builder ^com.mongodb.DBDecoderFactory (:db-decoder-factory connect-info)))
    (when (contains? connect-info :max-wait-time) (.maxWaitTime builder ^int (:max-wait-time connect-info)))
    (when (contains? connect-info :application-name) (.applicationName builder ^java.lang.String (:application-name connect-info)))
    (when (contains? connect-info :add-command-listener) (.addCommandListener builder ^com.mongodb.event.CommandListener (:add-command-listener connect-info)))
    (when (contains? connect-info :write-concern) (.writeConcern builder ^com.mongodb.WriteConcern (:write-concern connect-info)))
    (when (contains? connect-info :max-connection-life-time) (.maxConnectionLifeTime builder ^int (:max-connection-life-time connect-info)))
    (when (contains? connect-info :socket-timeout) (.socketTimeout builder ^int (:socket-timeout connect-info)))
    (when (contains? connect-info :threads-allowed-to-block-for-connection-multiplier) (.threadsAllowedToBlockForConnectionMultiplier builder ^int (:threads-allowed-to-block-for-connection-multiplier connect-info)))
    (when (contains? connect-info :read-preference) (.readPreference builder ^com.mongodb.ReadPreference (:read-preference connect-info)))
    (when (contains? connect-info :add-server-monitor-listener) (.addServerMonitorListener builder ^com.mongodb.event.ServerMonitorListener (:add-server-monitor-listener connect-info)))
    (.build builder)))

;; MongoClient
;; MongoClientOptions
;; MongoClientOptions$Builder
;; (com.mongodb.MongoCredential/createCredential "username" "dbname" (.toCharArray"password"))
(defn make-connection [connect-info]
  (cond
    (string? connect-info)
    (MongoClient. (MongoClientURI. connect-info))

    (map? connect-info)
    (let [client-options               (make-client-options connect-info)
          [server-type server-address] (connect-info->server-address connect-info)]
      (cond
        (= :singleton server-type)
        (MongoClient. ^ServerAddress server-address client-options)

        (= :list server-type)
        (MongoClient. ^java.util.List server-address client-options)

        :otherwise
        (throw (RuntimeException.
                (format "Error: don't know how to create a mongo connection out of: %s : %s"
                        (class connect-info)
                        connect-info)))))

    :otherwise
    (throw (RuntimeException.
            (format "Error: don't know how to create a mongo connection out of: %s : %s"
                    (class connect-info)
                    connect-info)))))

(defmacro with-connection [[conn-var-name connect-info] & body]
  `(with-open [~conn-var-name (make-connection ~connect-info)]
     ~@body))

(declare map->basic-db-object)

(defn val->basic-db-object [v]
  (cond
    ;; { :$date 1378857600000 }
    (and (map? v) (contains? v :$date)) (java.util.Date. ^long (:$date v))
    (map? v)                            (map->basic-db-object v)
    ;; (seqable? v)                        (mapv val->basic-db-object v)
    (vector? v)                         (mapv val->basic-db-object v)
    :otherwise                          v))

(defn map->basic-db-object ^BasicDBObject [m]
  (reduce
   (fn [^BasicDBObject acc [k v]]
     (.append acc
              (name k)
              (val->basic-db-object v)))
   (BasicDBObject.)
   m))

(defn seq->basic-db-objects
  ^{:tag (Class/forName "[Lcom.mongodb.BasicDBObject;")}
  [s]
  (into-array BasicDBObject (map map->basic-db-object s)))

(defn basic-db-object->map [obj]
  (reduce
   (fn [acc [k v]]
     (assoc acc (keyword k)
            (if (isa? (class v) BasicDBObject)
              (basic-db-object->map v)
              v)))
   {}
   obj))

(defn cursor->items [^DBCursor cursor]
  (loop [items []]
    (cond
      (not (.hasNext cursor))
      items
      
      :otherwise
      (recur (conj items (basic-db-object->map (.next cursor)))))))

;; NB: this is for the locally running, no-authentication, docker based mongodb
(def conn-info {:host                     "localhost"
                :port                     27017
                :server-selection-timeout 100
                :applicaiton-name         "my-test-app"})
(comment

  
  (with-connection [^MongoClient db conn-info]
    (-> db
        (.getDatabase "local")
        (.listCollectionNames)
        seq))

  ;; do we get an error?
  (with-connection [^MongoClient db (assoc conn-info :server-addresses [["localhost" 27018]])]
    (-> db (.getDatabase "local") (.listCollectionNames) seq))
  ;; yes, GREAT!
  

  ;; TODO: make sure we properly handle arrays, we do primitives + maps atm
  
  ;; insert example
  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   ;; what's the difference between .getDatabase and .getDB?
                   ;; (.getDatabase "test")
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      (.insert coll (seq->basic-db-objects
                     [{:name "MongoDB"
                       :type "database"
                       :count 1
                       :info {:x 203 :y 102}}]))))

  ;; first obj in colleciton
  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      (basic-db-object->map (.findOne coll))))

  ;; count of items in the collection
  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      (.getCount coll)))


  ;; get all the items in the collection
  ;; TODO: can we make a lazy sequence version of this?
  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))
          cursor (.find coll)]
      (cursor->items cursor)))

  ;; search by using a template, returns a cursor (result set)
  (with-connection [^MongoClient conn conn-info]
    (let [query-obj (map->basic-db-object {:name "MongoDB"})
          coll      (-> conn
                        (.getDB "test")
                        (.getCollection "my-docs"))]
      (cursor->items (.find coll query-obj))))

  ;; query using MongoDB operators.  Eg: $ne (not equal), $gt (greater than), etc
  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      (cursor->items (.find coll (map->basic-db-object {:name {"$ne" "MongoDB"}})))))


  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      (.remove coll (map->basic-db-object {:_id (ObjectId. "59ffc79d5275602b2bd573f8")}))))


  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      (.getIndexInfo coll)))
  ;; [{"v" 1, "key" {"_id" 1}, "name" "_id_", "ns" "test.my-docs"}]

  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      ;; NB: 1 is ascending, -1 is descending
      (.createIndex coll (map->basic-db-object {:name 1}))))
  
  ;; after creating the index, we now see the following returned from .getIndexInfo:
  ;; [{"v" 1, "key" {"_id" 1},  "name" "_id_",   "ns" "test.my-docs"}
  ;;  {"v" 1, "key" {"name" 1}, "name" "name_1", "ns" "test.my-docs"}]

  ;; create a full text index
  ;; query with $search or $text
  ;; http://mongodb.github.io/mongo-java-driver/2.13/getting-started/quick-tour-admin/#text-indexes:72db0d7d9b72569ab1f7da8f74305055
  ;; (.createIndex coll (map->basic-db-object {:field-name "text"}))

  ;; can also create geo indexes (2d)
  ;; http://mongodb.github.io/mongo-java-driver/2.13/getting-started/quick-tour-admin/#geo-indexes:72db0d7d9b72569ab1f7da8f74305055
  ;; https://docs.mongodb.com/manual/core/geospatial-indexes/


  ;; CRUD operations?
  ;; https://docs.mongodb.com/manual/crud/
  (with-connection [^MongoClient conn conn-info]
    (let [coll (-> conn
                   (.getDB "test")
                   (.getCollection "my-docs"))]
      (->
       coll
       (.find
        ;; 1st arg is the filter criteria aka query (where clause)
        (map->basic-db-object {:name {"$ne" "MongoDB.xxx"}})
        ;; 2nd arg here is the 'projection' (select clause)
        (map->basic-db-object {:name 1 :type 1 :info 1}))
       (.limit 5)
       cursor->items)))


  ;; https://stackoverflow.com/questions/9060860/how-to-check-the-available-free-space-in-mongodb


  )



