(ns perfortress.core
  (:require
   [schema.core           :as s]
   [cemerick.url          :as url]
   [clojure.tools.logging :as log]
   [perfortress.documents :as doc]
   [clj-time.core         :as time])
  (:import
   [com.perforce.p4java.server ServerFactory IServer]
   [com.perforce.p4java.server.callback ICommandCallback IProgressCallback]
   [com.perforce.p4java.option UsageOptions]
   [com.perforce.p4java.core CoreFactory]))

(def ServerInfo
  {(s/optional-key :protocol)    (s/enum "p4java" "p4jrpc" "p4javassl" "p4rpcssl" "p4jrpcntsssl")
   (s/required-key :hostname)    s/Str
   (s/optional-key :port)        s/Num
   (s/required-key :username)    s/Str
   (s/optional-key :password)    s/Str
   (s/optional-key :clientName)  s/Str
   (s/optional-key :autoConnect) (s/enum "y" "n")
   (s/optional-key :progName)    s/Str
   (s/optional-key :progVersion) s/Str})

(s/defn server-info->url [server-info :- ServerInfo]
  (let [u          (format "%s://%s:%s"
                           (:protocol server-info "p4java")
                           (:hostname server-info)
                           (:port server-info 1666))
        params     (select-keys server-info [:clientName :autoConnect :progName :progVersion])
        params     (merge {:progName    "libcljp4"
                           :progVersion "1.0"}
                          params)]
    (str u "?" (url/map->query params))))

(s/defn ->str [thing]
  (if (keyword? thing)
    (name thing)
    (str thing)))

(s/defn map->properties :- java.util.Properties [m :- {s/Any s/Any}]
  (reduce
   (fn [p [k v]]
     (.put p (->str k) v)
     p)
   (java.util.Properties.)
   m))

(def Client  {:client      IServer
              :server-info ServerInfo})

(s/defn set-client! [client :- Client client-name :- s/Str]
  (.setCurrentClient (:client client)
                     (.getClient (:client client) client-name)))

(s/defn connect! :- Client
  [server-info :- ServerInfo props :- {s/Any s/Any}]
  {:client      (let [client (ServerFactory/getServer (server-info->url server-info)
                                                      (map->properties props))]
                  (.setUserName client (:username server-info))
                  (.connect client)
                  client)
   :server-info server-info})


(s/defn login! [client :- Client]
  (cond
    (-> client :server-info :password)
    (.login (:client client) (-> client :server-info :password))
    
    ;; NB: this is ok if there is no authentication
    :otherwise
    :no-password))

(defmacro with-connection [[vname server-info] & body]
  `(let [~vname (connect! ~server-info {})]
     (try
       ~@body
       (catch Exception e#
         (try
           (.logout (:client ~vname))
           (catch Exception e#))
         (throw e#)))))

(s/defn exec-cmd [client :- Client cmd :- s/Str args :- [s/Str] input :- (s/maybe s/Str)]
  (let [args   (into-array String args)
        results (->
                 client
                 :client
                 (.execInputStringMapCmd cmd args input))]
    (map
     (fn [result]
       (reduce
        (fn [acc [k v]]
          (assoc acc (keyword k) v))
        {}

        result))
     results)))

(s/defn info [client :- Client args input]
  (exec-cmd client "info" args input))

(s/defn depots [client :- Client args input]
  (exec-cmd client "depots" args input))

(s/defn dirs [client :- Client args input]
  (exec-cmd client "dirs" args input))

(s/defn files [client :- Client args input]
  (exec-cmd client "files" args input))

(s/defn clients [client :- Client args input]
  (exec-cmd client "clients" args input))

;; .createClient
;; .updateClient
;; .deleteClient
;; .getChangelist
;; .getChangelists
;; .getChangelistFiles
;; .getFileContents aka print
;; .getProtectionEntries
;; .getUser
;; .getUsers
;; .getUserGroup
;; .getUserGroups
;; .createUser
;; .createUserGroup

(comment
  (defn ptest [cmd & [args input]]
    (let [server-info {:hostname   "192.168.99.100"
                       :port       1666
                       :username   "kburton"
                       :clientName (.. java.net.InetAddress getLocalHost getHostName)}]
      (with-connection [client server-info]
        (login! client)
        ;; NB: the client must already exist for this to work
        (when (:clientName server-info)
          (set-client! client (:clientName server-info)))
        (apply exec-cmd client [cmd (or args []) (or input nil)]))))

  (ptest "clients" ["-u" "kburton"])
  (.mkdirs (java.io.File. "p4-checkout"))
  (spit "p4-checkout/README" (str "This is a test file\nThe time is: %s\n" (time/now)))
  (ptest "add" ["p4-checkout/README"])

  (with-connection [client {:hostname   "192.168.99.100"
                            :port       1666
                            :username   "kburton"
                            ;; NB: the client must already exist for this to work
                            :clientName (.. java.net.InetAddress getLocalHost getHostName)}]
    (login! client)
    (def cc client)
    (set-client! client "KBURT1ML1"))

  (let [uname    (System/getenv "LOGNAME")
        hostname (.. java.net.InetAddress getLocalHost getHostName)
        pwd      (System/getProperty "user.dir")]
    (with-connection [client {:hostname "192.168.99.100"
                              :port     1666
                              :username "kburton"}]
      (login! client)
      #_(info client)
      #_(dirs client ["//*"])
      #_(files client ["//*"])
      #_(exec-cmd client "clients" [] nil)
      #_(clients client [] nil)
      #_(exec-cmd client "users" [] nil)
      #_(exec-cmd client "protects" ["-a"] nil)
      (exec-cmd client "client" ["-i"]
                (doc/map->spec
                 {:Client hostname
                  :Owner  uname
                  :Host   hostname
                  :Description [(str "Created by " uname)]
                  :Root        (str pwd "/p4-checkout")
                  :Options     "noallwrite noclobber nocompress unlocked nomodtime normdir"
                  :SubmitOptions "submitunchanged"
                  :LineEnd       "local"
                  :View          [(format "//depot/... //%s/..." hostname)]}))))

  (def p4-server-info {:hostname "192.168.99.100"
                       :port     1666
                       :username "kburton"})
  
  (def p4client (connect! p4-server-info {}))
  
  ;; ICommandCallback
  (do
    (.registerProgressCallback
     (:client p4client)
     (reify IProgressCallback
       (^void start [this ^int key]
        (log/infof "progress.start: key=%s" key))
       (^void stop [this ^int key]
        (log/infof "progress.stop: key=%s" key))
       (^boolean tick [this ^int key ^String tickMarker]
        (log/infof "progress.tick: key=%s tickMarker=%s" key tickMarker))))
    (.setUserName (:client p4client) (System/getenv "LOGNAME"))
    #_(.login (:client p4client) (-> "/Users/kburton/.p4pass" slurp (.split "\n") first)))
  
  (reify ICommandCallback
    #_(completedServerCommand [this ^int ikey ^long elapsedms]
        (log/infof "completedServerCommand %s %s" ikey elapsedms))
    (^void issuingServerCommand [this ^int ikey ^String cmdstring]
     (log/infof "issuingServerCommand %s %s" ikey cmdstring))
    (^void receivedServerErrorLine [this ^int ikey ^String errline]
     (log/infof "receivedServerErrorLine %s %s" ikey errline))
    (^void receivedServerInfoLine [this ^int ikey ^String infoline]
     (log/infof "receivedServerInfoLine %s %s" ^intikey ^String infoline))
    (^void receivedServerMessage [this ^int ikey ^int genericcode ^int severitycode^String msg]
     (log/infof "receivedServerMessage %s %s %s %s" ikey genericcode severitycode msg)))
  
  ;; (.login p4client "password")
  ;; (.logout) <== why?  >> create a with-client helper that manages the life-cycle?
  ;; .getServerInfo
  ;; .getDepots
  ;; .getDirectories
  ;; .getDepotsFiles
  ;; .isConnected
  ;; .getLoginStatus
  

  )



