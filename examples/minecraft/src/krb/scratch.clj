(ns krb.scratch
  (:import
   [net.canarymod Canary]
   [net.canarymod.api CanaryServer]
   [net.canarymod.api.world.blocks BlockType]
   [net.canarymod.api.world.position Location]
   [net.canarymod.api.entity EntityType]
   [com.pragprog.ahmine.ez EZPlugin]))

(defmacro after [stime & body]
  `(.run
    (Thread.
     (fn []
       (Thread/sleep ~stime)
       ((fn [] ~@body))))))

(defn ->player [p]
  (cond
   (string? p)
   (get-player p)

   (isa? (class p) net.canarymod.api.entity.living.humanoid.CanaryPlayer)
   p

   :otherwise
   (throw (RuntimeException. (format "Error: don't know how to convert %s into a player" p)))))



(defn get-player-list []
  (->
   (net.canarymod.Canary/getServer)
   (.getPlayerList)
   vec))


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
   (->player name)
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

;; build a "house"
;; dimensions: height, width, length

(defn build-cube-around-player [player radius btype]
  (let [player (->player player)
        loc    (.getLocation player)
        filled-plane (fn [loc r btype]
                       (let [world (.getWorld loc)]
                         (doseq [xx (range (* -1 radius) radius)
                                 zz (range (* -1 radius) radius)]
                           (let [loc (Location.
                                      (+ (.getX loc) xx)
                                      (- (.getY loc) 1)
                                      (+ (.getZ loc) zz))]
                             (.setBlockAt world loc btype)))))
        square-outline (fn [loc r btype]
                         (let [world (.getWorld loc)]
                           (doseq [xx (range (* -1 radius) radius)
                                   zz (range (* -1 radius) radius)]
                             (let [loc (Location.
                                        (- (.getY loc) 1)
                                        (+ (.getZ loc) zz))]
                               (.setBlockAt world loc btype)))))]
    ;; floor
    (filled-plane loc radius btype)
    ;; ceiling
    (let [roof-loc (Location. (.getX loc) (+ (.getY loc) (* 2 radius)) (.getZ loc))]
      (filled-plane roof-loc radius btype))

    ;; walls
    (let [world (.getWorld loc)]
      (doseq [yy (range (* 2 radius))
              zz (range (* -1 radius) radius)]
        (let [loc (Location.
                   (- (.getX loc) radius)
                   (+ (.getY loc) yy)
                   (+ (.getZ loc) zz))]
          (.setBlockAt world loc btype))))
    (let [world (.getWorld loc)]
      (doseq [yy (range (* 2 radius))
              zz (range (* -1 radius) radius)]
        (let [loc (Location.
                   (+ (.getX loc) radius)
                   (+ (.getY loc) yy)
                   (+ (.getZ loc) zz))]
          (.setBlockAt world loc btype))))

    (let [world (.getWorld loc)]
      (doseq [xx (range (* -1 radius) radius)
              yy (range (* 2 radius))]
        (let [loc (Location.
                   (+ (.getX loc) xx)
                   (+ (.getY loc) yy)
                   (+ (.getZ loc) radius))]
          (.setBlockAt world loc btype))))
    (let [world (.getWorld loc)]
      (doseq [xx (range (* -1 radius) radius)
              yy (range (* 2 radius))]
        (let [loc (Location.
                   (+ (.getX loc) xx)
                   (+ (.getY loc) yy)
                   (- (.getZ loc) radius))]
          (.setBlockAt world loc btype))))))

(defn all-mobs []
  (filter
   #(.isMob %)
   (->
    (get-player-list)
    first
    .getLocation
    .getWorld
    .getEntityLivingList
    vec)))

(defn light-em-up! []
  (doseq [mob (all-mobs)]
    (.setFireTicks mob 600)))

(defn bring-em-here! [loc]
  (doseq [mob (all-mobs)]
    (.teleportTo mob loc)))

(defn spawn-cow [loc]
  (let [world (.getWorld loc)]
    (EZPlugin/spawnEntityLiving loc EntityType/COW)))

(defn fling [player victim speed]
  (EZPlugin/fling (->player player) victim speed))

(defn mob-shooter [player n]
  (let [loc (player-location player)]
    (.setZ loc (+ 2 (.getZ loc)))
    (doseq [mob (take n (all-mobs))]
      (.teleportTo mob loc)
      (fling player mob 3.0))))

;; addSynchronousTask
;; removeSynchronousTask


(comment
  (after
   2000
   (mob-shooter "kyle_burton" 5))

  (let [loc (player-location "kyle_burton")]
    (.setY  loc (+ 5 (.getY loc)))
    (.setZ  loc (+ 5 (.getZ loc)))
    (spawn-cow loc))

  EZPlugin

  (.run
   (Thread.
    (fn []
      (Thread/sleep 100)
      (bring-em-here! (player-location "kyle_burton"))
      (light-em-up!))))

  (.run
   (Thread.
    #(let [loc (player-location "kyle_burton")]
       (Thread/sleep 2000)
       #_(.setY loc (+ (.getY loc) 10))
       (.setZ loc (+ (.getZ loc) 20))
       (bring-em-here! loc)
       #_(light-em-up!))))

  (light-em-up!)


  (let [loc (player-location "kyle_burton")]
    #_(Thread/sleep 2000)
    (.setY loc (+ (.getY loc) 40))
    (.setZ loc (+ (.getZ loc) 20))
    (bring-em-here! loc)
    #_(light-em-up!))



  (build-cube-around-player "kyle_burton" 10 BlockType/Air)

  (build-cube-around-player "kyle_burton" 4 BlockType/OakPlanks)

  (build-cube-around-player "kyle_burton" 2 BlockType/OakPlanks)

  (build-cube-around-player "kyle_burton" 4 BlockType/Ice)
  (build-cube-around-player "kyle_burton" 10 BlockType/Water)
  (build-cube-around-player "kyle_burton" 4 BlockType/Sponge)

  (doseq [ii (range 20)]
    (build-cube-around-player "kyle_burton" ii BlockType/Air))

  (doseq [ii (range 10)]
    (build-cube-around-player "kyle_burton" ii BlockType/Air))

  (build-cube-around-player "kyle_burton" 10 BlockType/JunglePlanks)
  (build-cube-around-player "kyle_burton" 10 BlockType/DarkOakPlanks)
  (build-cube-around-player "kyle_burton" 10 BlockType/Podzol)

  (build-cube-around-player "kyle_burton" 4 BlockType/CoalBlock)

  BlockType/SprucePlanks
  BlockType/BirchPlanks
  BlockType/JunglePlanks
  BlockType/AcaciaPlanks
  BlockType/DarkOakPlanks
  BlockType



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


  (net.canarymod.Canary/getServer)





  (->
   (get-player-list)
   first
   (.getLocation))


  net.canarymod.api.world.blocks.BlockType

  net.canarymod.api.world.position.Location

  net.canarymod.api.entity.living.humanoid.Player)
