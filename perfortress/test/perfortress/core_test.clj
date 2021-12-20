(ns perfortress.core-test
  (:require
   [clojure.test      :refer :all]
   [perfortress.core  :refer :all]
   [clojure.data.json :as json]))

;; NB: just assume the docker contianer has been launched
(defn test-config []
  (->
   "test-config.json"
   slurp
   (json/read-str :key-fn keyword)))

(comment
  (->>
   {:hostname   "192.168.99.100"
    :port       1666
    :username   "kburton"
    :clientName (.. java.net.InetAddress getLocalHost getHostName)}
   json/write-str
   (spit "test-config.json"))
  
  )

(defn test-server-info []
  (test-config))

(deftest test-info
  (let [server-info (test-server-info)]
    (with-connection [client server-info]
      (login! client)
      ;; NB: the client must already exist for this to work
      (when (:clientName server-info)
        (set-client! client (:clientName server-info)))
      (let [info (exec-cmd client "info" [] nil)]
        (def ii info)
        (is (not (zero? (count info))))))))

(deftest test-dirs
  (let [server-info (test-server-info)]
    (with-connection [client server-info]
      (login! client)
      ;; NB: the client must already exist for this to work
      (when (:clientName server-info)
        (set-client! client (:clientName server-info)))
      (let [res (exec-cmd client "dirs" ["//depot/*"] nil)]
        (def rr res)
        (is (not (zero? (count res))))))))


(comment

  (defn ptest [cmd & [args input]]
    (let [server-info (test-server-info)]
      (with-connection [client server-info]
        (login! client)
        ;; NB: the client must already exist for this to work
        (when (:clientName server-info)
          (set-client! client (:clientName server-info)))
        (apply exec-cmd client [cmd (or args []) (or input nil)]))))

  (with-connection [client (test-server-info)]
    (def cc2 client)
    ;; (ClientSpec->IClient client {:name (hostname)})
    (make-client client {:name (hostname)
                         :root (format "%s/tmp" (System/getProperty "user.dir"))}))

  (ptest "info" [])
  (ptest "users" [])
  (ptest "groups" [])
  (ptest "protects" [])
  (ptest "triggers" ["-o"])

  ;; NB: this just works?
  (let [triggers-table (com.perforce.p4java.impl.generic.admin.TriggersTable.)]
    (.getEntries triggers-table))

  (ptest "clients" ["-u" "kburton"])

  (.mkdirs (java.io.File. "p4-checkout"))
  (spit "p4-checkout/README" (str "This is a test file\nThe time is: %s\n" (time/now)))
  (ptest "add" ["p4-checkout/README"])


  (let [triggers (com.perforce.p4java.impl.generic.admin.TriggersTable.)
        entries  (java.util.ArrayList.)]
    (.add entries (com.perforce.p4java.impl.generic.admin.TriggerEntry. "mychk change-submit //depot/... \"echo %changelist% >> /tmp/pftrigger.log\"" 0))
    (.setEntries triggers entries)
    (com.perforce.p4java.impl.generic.core.InputMapper/map triggers))

  (with-connection [client {:hostname   "192.168.99.100"
                            :port       1666
                            :username   "kburton"
                            ;; NB: the client must already exist for this to work
                            :clientName (.. java.net.InetAddress getLocalHost getHostName)}]
    (login! client)
    (def cc client)
    (set-client! client "KBURT1ML1"))


  )

