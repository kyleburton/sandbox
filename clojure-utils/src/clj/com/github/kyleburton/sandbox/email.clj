(ns com.github.kyleburton.sandbox.email
  (:import [javax.mail Session Message Transport]
           [javax.mail.internet MimeMessage InternetAddress]
           [java.util Properties]))

(defn sendmail [args]
  (println (format "args=%s" args))
  (println (format "args=%s" (count args)))
  (println (format "args=%s" (apply hash-map args)))
  (let [args (apply hash-map args) 
        props (doto
                  (java.util.Properties.)
                (.put "mail.stp.host" (or (:host args) "localhost")))
        session (Session/getDefaultInstance props nil)
        msg (MimeMessage. session)]
    ;; assert that :from :to :subject and :body are present
    (.setFrom msg (InternetAddress. (:from args)))
    (if (seq? (:to args))
      (.setRecipients msg
                      javax.mail.Message$RecipientType/TO
                      (into-array (map #(InternetAddress. %) (:to args))))
      (.setRecipients msg javax.mail.Message$RecipientType/TO
                      (into-array [(InternetAddress. (:to args))])))
    (.setSubject msg (:subject args))
    (.setContent msg
                 (:body args)
                 (or (:content-type args) "text/plain"))
    (Transport/send msg)))

(defn sendmail* [& args]
  (sendmail args))


(comment
  (sendmail* :to "kyle.burton@gmail.com" 
             :from "mortis@asymmetrical-view.com"
             :subject "This is a test email from clojure."
             :body "I've send you an email from clojure.
I hope you like it.

You can download the code I used from here:
  http://github.com/kyleburton/sandbox/

Regards,

Kyle Burton
")

)