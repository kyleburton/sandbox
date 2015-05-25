(ns krb.scratch
  (:import
   [net.canarymod Canary]
   [net.canarymod.api CanaryServer]
   [net.canarymod.api.world.blocks BlockType]
   [com.pragprog.ahmine.ez EZPlugin]))


(defn get-player [name]
  (->
   (net.canarymod.Canary/getServer)
   (.getPlayer name)))

(defn chat-player [name & chats]
  (let [player (get-player name)]
    (doseq [chat chats]
      (.chat player chat))))

(defn player-location [name]
  (->
   (get-player name)
   (.getLocation)))


(defn cake-tower [name height & [block-type]]
  (let [block-type (or block-type (BlockType/Cake))]
   (when-let [player (get-player name)]
     (let [loc    (.getLocation player)]
       (chat-player name (format "cake-tower player=%s height=%s" name height))
       (.setX loc (int (+ 1 (int (.getX loc)))))
       (.setBlockAt (.getWorld loc) loc BlockType/OakWood)
       (chat-player name (format "loc x=%s y=%s z=%s"
                                 (.getX loc)
                                 (.getY loc)
                                 (.getZ loc)))
       (dotimes [ii height]
         (Thread/yield)
         (.setY loc (inc (int (.getY loc))))
         (chat-player name (format "loc x=%s y=%s z=%s"
                                   (.getX loc)
                                   (.getY loc)
                                   (.getZ loc)))
         (->
          loc
          (.getWorld)
          (.setBlockAt loc block-type)))))))

(defn location-compass-direction [loc]
  (let [rotation (.getRotation loc)]
    (cond
     (and (>= rotation 0.0)
          (< rotation 90.0))
     :south
     
     (and (>= rotation 90.0)
          (< rotation 180.0))
     :west
     
     (and (>= rotation 180.0)
          (< rotation 270.0))
     :north
     
     (>= rotation 270.0)
     :east)))

(defn player-compass-direction [name]
  (->
   name
   get-player
   .getLocation
   location-compass-direction))


(defn one-in-front-of-direction [direction]
  ({:north [ 0  0 -1]
    :east  [ 1  0  0]
    :west  [-1  0  0]
    :south [ 0  0  1]} direction))

(defn one-in-front-of-location! [loc direction]
  (let [v (one-in-front-of-direction direction)]
    (.setX loc (nth v 0))
    (.setZ loc (nth v 2))
    loc))

(comment
  (->
   "kyle_burton"
   player-location
   (one-in-front-of-location! :north))
  
  (cake-tower "kyle_burton" 3)
  (cake-tower "kyle_burton" 3 BlockType/SlimeBlock)
  (cake-tower "kyle_burton" 3 BlockType/BlackCarpet)

  (player-compass-direction "kyle_burton")

  (get-player "kyle_burton")
  (player-location "kyle_burton")
  
  (get-player "kyle_burton")

  (chat-player "kyle_burton" "Hey" "you" "thing!")

  (.getHelp (net.canarymod.Canary/help))

  net.canarymod.Canary

  
  net.canarymod.api.CanaryServer
  (net.canarymod.Canary/getServer)


  (->
   (net.canarymod.Canary/getServer)
   (.getPlayerList)
   vec
   first
   (.chat "yep"))


  (->
   (net.canarymod.Canary/getServer)
   (.getPlayerList)
   vec
   first
   (.getLocation))

  



  net.canarymod.api.world.blocks.BlockType

  net.canarymod.api.world.position.Location

  net.canarymod.api.entity.living.humanoid.Player
  )
