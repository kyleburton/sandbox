(ns the-mystery-on-orville-st.shell
  (:require
   [clojure.tools.logging             :as log]
   [the-mystery-on-orville-st.game    :as game]
   [the-mystery-on-orville-st.actions :as actions]))

(defn -main [& args]
  (log/infof "Hi there, lets start a readline shell...args=%s" args)
  (cond
    (-> args first (= "orville"))
    (game/init!)

    ;; (-> args first (= "game2"))
    ;; (game2/init!)

    :otherwise
    (do
      (log/infof "Sorry, not sure what you wanted me to run?")
      (log/infof "Try one of:")
      (log/infof "")
      (log/infof "   lein run -m the-mystery-on-orville-st.shell orville")
      (log/infof "   lein run -m the-mystery-on-orville-st.shell game2")
      (log/infof "")
      (System/exit -1)))

  (.print System/out (actions/command "help"))
  (.print System/out "\n")
  (.print System/out (actions/command "look"))
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
