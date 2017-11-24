(ns the-foster-mystery.actions
  (:require
   [the-mystery-on-orville-st.data    :as orville-data    :refer []]
   [the-mystery-on-orville-st.actions :as orville-actions :refer [register-action! map->Action help command]]
   [the-foster-mystery.data           :as foster-data]
   [clojure.string                    :as string]
   [schema.core                       :as s]
   [clojure.tools.logging             :as log]))

(s/defn init! []
  (orville-actions/init!)
  ;; This is what registering an action looks like, see the help
  ;; function for how to write one :)
  
  ;; (register-action! (map->Action {:name        :help
  ;;                                 :aliases     #{"help"
  ;;                                                "help me"
  ;;                                                "what can i do"
  ;;                                                "what can i do here"
  ;;                                                "what should i do"}
  ;;                                 :description ["Get some help."]
  ;;                                 :action-fn   #'help}))
  )


