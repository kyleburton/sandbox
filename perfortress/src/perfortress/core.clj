(ns perfortress.core
  (:require
   [schema.core           :as s]
   [cemerick.url          :as url]
   [clojure.tools.logging :as log]
   ;; [perfortress.documents :as doc]
   [clj-time.core         :as time])
  (:import
   [com.perforce.p4java.server ServerFactory IServer]
   [com.perforce.p4java.server.callback ICommandCallback IProgressCallback]
   [com.perforce.p4java.option UsageOptions]
   [com.perforce.p4java.core CoreFactory]
   [com.perforce.p4java.impl.mapbased.client Client]
   [com.perforce.p4java.client IClient]
   [com.perforce.p4java.client IClientSummary$ClientLineEnd]))


;; TODO: need the ability to set the socket timeout on the underlying connection (lets hope that p4java exposes that capability :/)

(defn hostname-impl []
  (.. java.net.InetAddress getLocalHost getHostName))

(def hostname (memoize hostname-impl))

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

(def P4Client  {:client      IServer
                :server-info ServerInfo})

(s/defn set-client! [client :- P4Client client-name :- s/Str]
  (.setCurrentClient (:client client)
                     (.getClient (:client client) client-name)))

(s/defn connect! :- P4Client
  [server-info :- ServerInfo props :- {s/Any s/Any}]
  {:client      (let [client (ServerFactory/getServer (server-info->url server-info)
                                                      (map->properties props))]
                  (.setUserName client (:username server-info))
                  (.connect client)
                  client)
   :server-info server-info})


(s/defn login! [client :- P4Client]
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

(s/defn exec-cmd [client :- P4Client cmd :- s/Str args :- [s/Str] input :- (s/maybe s/Str)]
  (let [args    (into-array String args)
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

(s/defn info [client :- P4Client args input]
  (exec-cmd client "info" args input))

(s/defn depots [client :- P4Client args input]
  (exec-cmd client "depots" args input))

(s/defn dirs [client :- P4Client args input]
  (exec-cmd client "dirs" args input))

(s/defn files [client :- P4Client args input]
  (exec-cmd client "files" args input))

(s/defn clients [client :- P4Client args input]
  (exec-cmd client "clients" args input))

(s/defn sizes [client :- P4Client args input]
  (exec-cmd client "sizes" args input))

(defn seq->list [s]
  (reduce
   (fn [l elt]
     (.add l elt)
     l)
   (java.util.ArrayList.)
   s))


(def ViewSpec [(s/one s/Str "depot spec") (s/one s/Str "client-spec")])
(def ClientSpec
  {(s/required-key :name)        s/Str
   (s/optional-key :description) s/Str
   (s/optional-key :hostname)    s/Str
   (s/optional-key :owner-name)  s/Str
   (s/required-key :root)        s/Str
   (s/optional-key :view)        [ViewSpec]})


;; (s/validate [(s/one s/Str "depot spec") (s/one s/Str "client-spec")] ["foo" "bar"])

(defn vec->List [v]
  (reduce
   (fn [l elt]
     (.add l elt)
     l)
   (java.util.ArrayList.)
   v))


(s/defn ClientSpec->IClient [client :- P4Client client-spec :- ClientSpec]
  (def cc client)
  (Client.
   (:name client-spec)
   nil ;; date accessed
   nil ;; date updated
   (:description client-spec "client created by perfortress")
   (:hostname client-spec (hostname))
   (:owner-name client-spec (System/getProperty "user.name"))
   (:root client-spec)
   (:line-end client-spec IClientSummary$ClientLineEnd/LOCAL)
   ;; TODO: add this into ClientSpec
   (com.perforce.p4java.impl.generic.client.ClientOptions.
    false ;; all-write
    false ;; clobber
    false ;; compress
    false ;; locked
    false ;; modtime
    false ;; rmdir
    )
   (com.perforce.p4java.impl.generic.client.ClientSubmitOptions. "submitunchanged")
   (java.util.ArrayList.) ;; alternate roots
   (:client client)       ;; server-impl
   (com.perforce.p4java.impl.generic.client.ClientView.
    (com.perforce.p4java.impl.mapbased.client.Client.)
    (vec->List (map (fn [[depot-path local-path]]
                      (com.perforce.p4java.impl.generic.client.ClientView$ClientViewMapping.
                       0
                       depot-path
                       local-path))
                    (:view client-spec))))))

(s/defn make-client [client :- P4Client client-spec :- ClientSpec]
  (.createClient (:client client) (ClientSpec->IClient client client-spec)))


(s/defn paths->changelist [paths :- [s/Str]]
  (com.perforce.p4java.core.file.FileSpecBuilder/makeFileSpecList paths))

(defn seq->file-specs [l]
  (mapv (fn [e]
          (com.perforce.p4java.impl.generic.core.file.FileSpec. e))
        l))

(defn get-file-contents [cli path]
 (->
  cli
  :client
  (.getFileContents (seq->file-specs [path]) false true)
  slurp))

