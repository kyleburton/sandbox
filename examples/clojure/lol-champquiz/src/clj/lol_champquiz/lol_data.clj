(ns lol-champquiz.lol-data
  (:require
   [org.httpkit.client :as http]
   [clojure.data.json :as json]))

(def urls {:list-of-champions-page "http://leagueoflegends.wikia.com/wiki/List_of_champions"
           :data-dragon-champions "http://ddragon.leagueoflegends.com/cdn/6.8.1/data/en_US/champion.json"})

(defn config []
  (->
   (str (System/getProperty "user.home") "/.riot.d/developer.riotgames.com.json")
   slurp
   (json/read-str :key-fn keyword)))

;; https://s3-us-west-1.amazonaws.com/riot-api/seed_data/matches1.json
;; curl --request GET 'https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/RiotSchmick?api_key=<key>' --include
(defn all-champions-impl [opts]
  ;; /api/lol/{region}/v1.2/champion
  (http/get (str "https://na.api.pvp.net/api/lol/na/v1.2/champion?api_key="
                 (-> opts :api-key))))

(defonce all-champions (memoize all-champions-impl))

(defn champion-by-id-impl [champion-id opts]
  ;; /api/lol/{region}/v1.2/champion
  (http/get (str "https://na.api.pvp.net/api/lol/na/v1.2/champion/" champion-id "?api_key="
                 (-> opts :api-key))))

(defonce champion-by-id (memoize champion-by-id-impl))

(defn summoner-by-name-impl [summoner-name opts]
  (http/get (str "https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/"
                 summoner-name
                 "?api_key="
                 (-> opts :api-key))))


(comment

  (def res (all-champions (config)))
  @res
  (->
   res
   deref
   :body
   (json/read-str :key-fn keyword)
   :champions
   count)

  (->
   (champion-by-id 266 (config))
   deref
   :body
   (json/read-str :key-fn keyword))

  )


