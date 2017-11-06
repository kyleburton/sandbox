(ns mongo-start.admin
  (:require
   [mongo-start.mongo      :refer [with-connection conn-info
                                   seq->basic-db-objects
                                   basic-db-object->map
                                   map->basic-db-object]]
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


(comment


  (with-connection [^MongoClient conn conn-info]
    (-> conn
        (.getDatabase "test")
        (.runCommand (map->basic-db-object {:rolesInfo      1
                                            :showPrivileges true}))))
  ;; => {"roles" [], "ok" 1.0}

  (with-connection [^MongoClient conn conn-info]
    (-> conn
        (.getDatabase "test")
        (.runCommand (map->basic-db-object {:usersInfo       1
                                            :showCredentials 1
                                            ;; :showPrivileges  1
                                            }))))
  
  ;; {"users" [
  ;;           {"_id"         "test.a-user",
  ;;            "user"        "a-user",
  ;;            "db"          "test",
  ;;            "credentials" {"SCRAM-SHA-1" {"iterationCount" 10000, "salt" "X8rJeveCL1xsVeHfYmtxGQ==", "storedKey" "codKUpcZ92l+XSAFmQ7y/hfwwqk=", "serverKey" "iyiy65cbktXw2Qkr9X55UcMNZBE="}},
  ;;            "customData"  {"firstName" "A", "lastName" "User"},
  ;;            "roles"       []}]
  ;;  "ok" 1.0}
  
  ;; => {"users" [], "ok" 1.0}


  (with-connection [^MongoClient conn conn-info]
    (->
     conn
     (.getDatabase "test")
     (.runCommand
      (map->basic-db-object
       {:createUser "a-user"
        :pwd        "some-password"
        :customData {:firstName "A"
                     :lastName  "User"}
        :roles      []}))))

  
  )
