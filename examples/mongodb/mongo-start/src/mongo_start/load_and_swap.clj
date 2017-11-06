(ns mongo-start.load-and-swap
  (:require
   [mongo-start.mongo      :refer [with-connection conn-info
                                   seq->basic-db-objects
                                   basic-db-object->map]]
   [clojure.data.json      :as json]
   [clojure.tools.logging  :as log])
  (:import
   [com.mongodb               MongoClient   MongoClientURI ServerAddress
    MongoClientOptions MongoClientOptions$Builder
    ServerAddress BasicDBObject DBCursor]
   [com.mongodb.client        MongoDatabase MongoCollection MongoCursor]
   [com.mongodb               Block]
   [com.mongodb.client.result DeleteResult UpdateResult]
   [org.bson.types ObjectId]))

(def mongo-sample-data-set-url "https://raw.githubusercontent.com/mongodb/docs-assets/primer-dataset/primer-dataset.json")
(def get-mongo-sample-data (memoize #(slurp mongo-sample-data-set-url)))

(def mongo-sample-data-set
  (->>
   (->
    ^String (get-mongo-sample-data)
    (.split "\n"))
   (map #(json/read-str % :key-fn keyword))))

(comment
  
  ;; Lets run a test:
  ;; * import a sample data set into collection.1
  ;; * start a background thread that continuously reads from collection.1
  ;; * concurrently, load the same dataset into collection.2
  ;; * rename collection.2 to collection.1

  (with-connection [^MongoClient conn conn-info]
    (-> conn
        (.getDB "test")
        (.getCollection "collection.2")
        (.drop))
    (-> conn
        (.getDB "test")
        (.getCollection "collection.1")
        (.drop)))

  (time
   (with-connection [^MongoClient conn conn-info]
     (let [coll        (-> conn
                           (.getDB "test")
                           (.getCollection "collection.1"))
           nrecs       (count mongo-sample-data-set)
           mid-point   (int (/ nrecs 2))
           first-half  (take mid-point mongo-sample-data-set)
           second-half (drop mid-point mongo-sample-data-set)]
       (doseq [recs (partition 10 first-half)]
         (.insert coll (seq->basic-db-objects recs))))))
  ;; 2124ms

  (def stop (atom nil))
  (.start
   (Thread. (fn []
              (with-connection [^MongoClient conn conn-info]
                (loop []
                  (if @stop
                    (do
                      (log/infof "DONE"))
                    (let [coll (-> conn
                                   (.getDB "test")
                                   (.getCollection "collection.1"))]
                      (log/infof "count=%s; first document: %s"
                                 (.getCount coll)
                                 (basic-db-object->map (.findOne coll)))
                      (recur))))))))

  (reset! stop true)
  
  (time
   (with-connection [^MongoClient conn conn-info]
     (let [coll        (-> conn
                           (.getDB "test")
                           (.getCollection "collection.2"))
           nrecs       (count mongo-sample-data-set)
           mid-point   (int (/ nrecs 2))
           first-half  (take mid-point mongo-sample-data-set)
           second-half (drop mid-point mongo-sample-data-set)]
       (doseq [recs (partition 10 second-half)]
         (.insert coll (seq->basic-db-objects recs))))))

  ;; 3700 ms
  (with-connection [^MongoClient conn conn-info]
    (let [coll        (-> conn
                          (.getDB "test")
                          (.getCollection "collection.2"))]
      (.rename coll "collection.1" true)))

  )
