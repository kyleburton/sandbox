(ns the-foster-mystery.shell
  (:require
   [clojure.tools.logging :as log]
   ;; [the-mystery-on-orville-st.game :as game]
   [the-mystery-on-orville-st.actions :as actions]
   [the-foster-mystery.game :as game]
   [clojure.string :as string]))

(defn -main [& args]
  (log/infof "Hi there, lets start a readline shell...")
  (game/init!)
  (.print System/out (actions/command "help"))
  (.print System/out "\n")
  ;;(.print System/out (actions/command "look"))
  (.print System/out (string/join "\n  "
                                  ["It's June 7th, 1901. Master Will was said to be killed"
                                   "the night before. As the detective, you must go around"
                                   "the house and interview everyone to solve the mystery of"
                                   "who killed Master Will Foster."]))
  (.print System/out "\n")
  (let [prompt "> "
        rdr    (java.io.BufferedReader. (java.io.InputStreamReader. System/in))]
    (.print System/out prompt)
    (.flush System/out)
    (loop [prompt "> "
           line   (.readLine rdr)]
      (cond
        (or (nil? line) (.startsWith (.toLowerCase line) "quit"))
        :ok

        :otherwise
        (do
          (.print System/out (actions/command line))
          (.print System/out "\n")
          (.print System/out prompt)
          (.flush System/out)
          (recur prompt (.readLine rdr)))))))


(comment

  (actions/command "look")
  )
