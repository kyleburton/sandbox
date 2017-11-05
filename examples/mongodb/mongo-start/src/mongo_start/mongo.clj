(ns mongo-start.mongo
  (:require
   [clojure.data.json :as json]
   [clojure.string    :as string])
  (:import
   [com.mongodb               MongoClient   MongoClientURI ServerAddress MongoClientOptions
    ServerAddress]
   [com.mongodb.client        MongoDatabase MongoCollection MongoCursor]
   [com.mongodb               Block]
   [com.mongodb.client.result DeleteResult UpdateResult]))

;; static com.mongodb.client.model.Filters.*;
;; static com.mongodb.client.model.Updates.*;

;; https://stackoverflow.com/questions/29030526/mongoclient-not-respecting-connecttimeout
;; MongoClientOptions.builder().serverSelectionTimeout(500).build()
;; connectTimeout
;; socketTimeout
;; heartbeatConnectTimeout
;; heartbeatSocketTimeout

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

(defn make-connection [connect-info]
  (cond
    (string? connect-info)
    (MongoClient. (MongoClientURI. connect-info))

    (map? connect-info)
    (let [client-options        (com.mongodb.MongoClientOptions/builder)
          [server-type server-address] (connect-info->server-address connect-info)]
      (when (contains? connect-info :server-selection-timeout)
        (.serverSelectionTimeout client-options (:server-selection-timeout connect-info)))
      (cond
        (= :singleton server-type)
        (MongoClient. ^ServerAddress server-address (.build client-options))

        (= :list server-type)
        (MongoClient. ^java.util.List server-address (.build client-options))

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

(comment
  (with-connection [db {:host "localhost"
                        :port 27017
                        :server-selection-timeout 100}]
    (-> db (.getDatabase "local") (.listCollectionNames) seq))
  
  (->
   (com.mongodb.MongoClientOptions/builder)
   (.serverSelectionTimeout 100)
   .build)

  (ServerAddress. "localhost" 27017)

  ;; (MongoCLient. seeds :- [ServerAddress] options :- MongoClientOptions)
  ;; (MongoCLient. server-address :- ServerAddress options :- MongoClientOptions)
  
  (with-connection [db "mongodb://localhost:27017"]
    (-> db (.getDatabase "local") (.listCollectionNames) seq))

  (with-connection [db "mongodb://localhost:27018"]
    (-> db (.getDatabase "local") (.listCollectionNames) seq))

  ;; NB: these don't fail when the host:port don't point to a live mongo :/
  ;; TODO: how do we get them to hard error when the mongo isn't real/reachable?
  (MongoClient. "localhost" 27018)
  
  (MongoClient. (MongoClientURI. "mongodb://localhost:27018"))


  (def client (->  "mongodb://localhost:27017" MongoClientURI. MongoClient.))

  (.getDatabase client "test")

  ;; TODO: ok, looks like this doesn't error either, which sucks :/
  (.getDatabase client "nothing")

  ;; TODO: still no error here either
  (->
   client
   (.getDatabase "nothing")
   (.getCollection "still.nothing"))


  (seq (.listDatabases client))
  ({"name" "local", "sizeOnDisk" 8.388608E7, "empty" false})

  (->
   client
   (.getDatabase "local")
   .listCollectionNames
   seq)
  ("startup_log" "system.indexes")

  

  )
