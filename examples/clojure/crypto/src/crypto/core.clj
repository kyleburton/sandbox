;; Example AES encryption / decryption, based on example from: http://jyliao.blogspot.com/2010/08/exploring-java-aes-encryption-algorithm.html
(ns crypto.core
  (:import [javax.crypto KeyGenerator SecretKey Cipher]
           [javax.crypto.spec SecretKeySpec]
           [java.io File FileOutputStream DataInputStream FileInputStream]
           [java.util Properties]
           [org.apache.commons.codec.binary Base64])
  (:use
   [clj-etl-utils.lang-utils :only [raise]]))


;; goal: example of AES key based encryption / decryption


(def encode-base64
     (let [b (Base64.)]
       (fn encode-base64 [raw]
         (.encode b raw))))

(def decode-base64
     (let [b (Base64.)]
       (fn decode-base64 [coded]
         (.decode b coded))))


(defn key->file [rawkey filename]
  ( let [out (new FileOutputStream (File. filename))]
    (do (.write out rawkey)
        (.close out))))

(defn file->key [filename]
  (let [file   (File. filename)
        rawkey (byte-array (.length file))
        in     (new DataInputStream (FileInputStream. file))]
    (do (.readFully in rawkey)
        (.close in )
        rawkey)))


(defn get-propfile [filename]
  (let [prop (new Properties)]
    (do (.load prop (FileInputStream. filename)))
    prop))


(defn genkey [keygen]
  (do (.init keygen  128)
      (.getEncoded (.generateKey keygen) )))

(defn do-encrypt [rawkey plaintext]
  (let [cipher (Cipher/getInstance "AES")]
    (do (.init cipher Cipher/ENCRYPT_MODE (SecretKeySpec. rawkey "AES"))
        (.doFinal cipher (.getBytes plaintext)))))

(defn do-decrypt [rawkey ciphertext]
  (let [cipher (Cipher/getInstance "AES")]
    (do (.init cipher Cipher/DECRYPT_MODE (SecretKeySpec. rawkey "AES"))
        (String. (.doFinal cipher ciphertext)))))


(defn get-password [key rawkey filename]
  (let [ props (get-propfile filename)
        coded  (.getProperty props key)
        cipher (decode-base64 coded)]
    (do (do-decrypt rawkey cipher))))

(defn string->key [s]
  (if (< (count s) 16)
    (raise "Error: key must be >= 16 characters to be a valid AES key, you supplied s[%d]='%s'" (count s) s))
  (.getBytes (.substring s 0 16) "UTF-8"))

(comment "Example usage"

         (alength (genkey (KeyGenerator/getInstance "AES")))

         (def *key* (string->key "some key with some more padding"))

         (alength *key*)

         (do-decrypt *key* (do-encrypt *key* "this is some plaintext"))

         (get-password "jms" (readkey "test.key") "data.out")

         )