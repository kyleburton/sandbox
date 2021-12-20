(ns pbkdf2.core
  (:require
   [clojure.tools.nrepl.server                 :refer [start-server stop-server]]
   [cider.nrepl                                :refer [cider-nrepl-handler]]
   [clojure.tools.logging                      :as log]
   [schema.core                                :as s]))

;; https://www.owasp.org/index.php/Hashing_Java
;; http://howtodoinjava.com/security/how-to-generate-secure-password-hash-md5-sha-pbkdf2-bcrypt-examples/#PBKDF2WithHmacSHA1


(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4002}}))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config)
  (s/set-fn-validation! true))

(s/defn gen-salt []
  (let [sr(java.security.SecureRandom/getInstance "SHA1PRNG")
        buff (byte-array 16)]
    (.nextBytes sr buff)
    buff))

(s/defn bytes->hex [b :- (Class/forName "[B")]
  (let [bi      (BigInteger. 1 ^bytes b)
        s       (.toString bi 16)
        pad-len (- (* 2 (count b) (count s)))]
    (if (pos? pad-len)
      (str (format "%0" pad-len "d" 0) s)
      s)))

(s/defn hash-password [s :- s/Str]
  (let [password-chars    (.toCharArray ^String s)
        salt              (gen-salt)
        num-iterations    1000 ;; aka work-factor
        sha-512-byte-size 64 ;; NB: sha512 is 64 bits
        keylength         (* 8 sha-512-byte-size)
        skf               (javax.crypto.SecretKeyFactory/getInstance "PBKDF2WithHmacSHA512")
        spec              (javax.crypto.spec.PBEKeySpec. password-chars salt num-iterations keylength)]
    (->
     skf
     (.generateSecret spec)
     .getEncoded)))

(s/defn hash-password->hex [s :- s/Str]
  (->
   s
   hash-password
   bytes->hex))

(comment
  (hash-password->hex "banana")
  "bc14ee6c3126c5fd74116f4b5e056f95e49782c7755d76a6e89fc1cacb088d582567febfd7171a6388ae04442c59f74dd5e1d6185ff309d54fc88a9d042828d0"
  (hash-password->hex "Banana")
  "a173eaebd615096b5b6809813df0d249dc6c87c268e7e80aa37863e31dd64c6e19ad470856aedfd9ab330b2daa0eae974e7377283260bdd364a603d23441dc5b"

  (hash-password->hex "banana pants umbrella dreams")
  "2048ef3237ed8088d3988f2bd7c1fc1b18afd35a989cfd89c75697bb976f7ea66a054a6e9ba60e853e282dd1ca321bbdef031c8d3f690e809325b0444f46a5f1"

  ;; 
  ;; SecretKeyFactory skf = SecretKeyFactory.getInstance ( "PBKDF2WithHmacSHA512");
  ;; PBEKeySpec spec = new PBEKeySpec ( password, salt, iterations, keyLength);
  ;; SecretKey key = skf.generateSecret ( spec);
  ;; byte [] res = key.getEncoded ();
  ;; return res;
   
)
