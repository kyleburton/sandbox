(ns krb.scratch
  (:import
   [net.canarymod Canary]
   [net.canarymod.api CanaryServer]
   [net.canarymod.api.world.blocks BlockType]
   [net.canarymod.api.world.position Location]
   [net.canarymod.api.entity EntityType]
   [com.pragprog.ahmine.ez EZPlugin]
   #_[schema.core :as s]))

(defmacro after [stime & body]
  `(.run
    (Thread.
     (fn []
       (Thread/sleep ~stime)
       ((fn [] ~@body))))))

(defn get-player [name]
  (->
   (net.canarymod.Canary/getServer)
   (.getPlayer name)))

(defn player-names []
  (->
   (net.canarymod.Canary/getServer)
   .getPlayerNameList
   vec))

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

(defn all-creatures []
  (->
   (get-player-list)
   first
   .getLocation
   .getWorld
   .getEntityLivingList
   vec))

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


(defn light-em-all-up! []
  (doseq [creature (all-creatures)]
    (.setFireTicks creature 600)))

(defn light-em-all-up! []
  (doseq [creature (all-creatures)]
    (.setFireTicks creature 600)))

(defn bring-em-here! [loc]
  (doseq [mob (all-mobs)]
    (.teleportTo mob loc)))

(defn bring-em-all-here! [loc]
  (doseq [creature (all-creatures)]
    (.teleportTo creature loc)))

(defn spawn-cow [loc]
  (let [world (.getWorld loc)]
    (EZPlugin/spawnEntityLiving loc EntityType/COW)))

(defn fling [player victim speed]
  (EZPlugin/fling (->player player) victim speed))

(defn mob-shooter [player & [n]]
  (.run
   (Thread.
    #(let [player-name "kyle_burton"
           player      (->player player-name)
           loc         (player-location player)
           n           (or n Integer/MAX_VALUE)]
       (Thread/sleep 2000)
       (doseq [mob (take n (all-mobs))]
         (Thread/sleep 50)
         (.teleportTo mob loc)
         (fling player mob 3.0))))))

(defn creature-shooter [player entity-type num]
  (let [player      (->player player)
        loc         (player-location player)]
    (doseq [entity-type (take num (repeat entity-type))]
      (Thread/sleep 50)
      (let [thing (-> (Canary/factory)
                      .getEntityFactory
                      (.newEntity entity-type loc))]
        (.spawn thing)
        (.println System/out (format "creating entity: %s at %s" entity-type loc))
        (fling player thing 3.0)))))

(comment


  (creature-shooter "kyle_burton" EntityType/DONKEY 5)
  (creature-shooter "kyle_burton" EntityType/FARMER 5)
  (creature-shooter "kyle_burton" EntityType/GIANTZOMBIE 5)
  (creature-shooter "kyle_burton" EntityType/IRONGOLEM 5)
  (creature-shooter "kyle_burton" EntityType/OCELOT 5)
  (creature-shooter "kyle_burton" EntityType/COW 100)

  (creature-shooter "kyle_burton" EntityType/MULE 5)
  (creature-shooter "kyle_burton" EntityType/PRIEST 5)
  (creature-shooter "kyle_burton" EntityType/SHEEP 5)
  (creature-shooter "kyle_burton" EntityType/WOLF 12)

  (do
    (creature-shooter "kyle_burton" EntityType/SNOWMAN 12)
    (creature-shooter "kyle_burton" EntityType/CREEPER 12))

  (do
    (creature-shooter "kyle_burton" EntityType/SNOWMAN 12)
    (creature-shooter "kyle_burton" EntityType/IRONGOLEM 12))


  (creature-shooter "kyle_burton" EntityType/ZOMBIE 12)
  (creature-shooter "kyle_burton" EntityType/SNOWMAN 5)

  (creature-shooter "kyle_burton" EntityType/ENDERMAN 5)

  (light-em-all-up!)

  ;; (creature-shooter "kyle_burton" EntityType/TNTPRIMED 2)
  ;; (creature-shooter "kyle_burton" EntityType/LARGEFIREBALL 10)

  (light-em-up!)

  EntityType

  )

;; addSynchronousTask
;; removeSynchronousTask

(defn ->location [loc]
  (cond
   (or (vector? loc) (seq? loc))
   (Location. (nth loc 0)
              (nth loc 1)
              (nth loc 2))
   (map? loc)
   (Location. (:x loc) (:y loc) (:z loc))
   :else
   loc))

(defn ->location-tuple [loc]
  (cond
   (or (vector? loc) (seq? loc))
   loc
   (map? loc)
   [(long (:x loc)) (long (:y loc)) (long (:z loc))]
   :else
   [(long (.getX loc)) (long (.getY loc)) (long (.getZ loc))]))

(defn loc->ground-height [loc]
  (let [loc    (->location loc)
        world  (.getWorld loc)
        height (.getHighestBlockAt world (.getY loc) (.getZ loc))]
    ;; should we try not to place the user undeground?
    (if (< height 30)
      60
      height)))

(defn loc-set-at-ground-height! [loc]
  (.setY loc (loc->ground-height loc))
  loc)

(defn teleport-to-ground-height [player location]
  (let [loc (->location location)]
    (loc-set-at-ground-height! loc)
    (.teleportTo (->player player) loc)))

(defn teleport-to [player loc]
  (.teleportTo (->player player)
               (->location loc)))

;; l2 must be a map for now
;; TODO: allow l2 to be other kinds of locations (vector of offsets or map)
(defn loc+ [l1 l2]
  (let [l1 (->location l1)
        l2 (->location l2)]
    (Location.
     (+ (.getX l1) (.getX l2))
     (+ (.getY l1) (.getY l2))
     (+ (.getZ l1) (.getZ l2)))))

(defn loc->vec [l]
  [(.getX l)
   (.getY l)
   (.getZ l)])

;; TODO: build east/west
;; TODO: if called twice, don't keep layerying up half slabs
;; TODO: every 10 blocks, place light posts (2 stacked fenceposts, and ... glowstone?)
(defn build-road-system [start-loc distance & [road-info]]
  (let [start-loc (->
                   start-loc
                   ->location
                   loc-set-at-ground-height!)
        length    (or (:length road-info) distance)
        pavement-type (or (:block-type road-info)
                          BlockType/StoneSlab)
        world (.getWorld start-loc)]
    
    ;; build north/south
    (doseq [xx (range (* -1 length) length)]
      (let [l1 (loc+ start-loc {:x xx})
            l2 (loc+ l1        {:z 1})]
        (.println System/out (format "laying down %s at %s and %s" pavement-type (loc->vec l1) (loc->vec l2)))
        (.setBlockAt world l1 pavement-type)
        #_(.setBlockAt world (loc+ l1 [0 1 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l1 [0 2 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l1 [0 3 0]) BlockType/Air)
        
        (.setBlockAt world l2 pavement-type)
        #_(.setBlockAt world (loc+ l2 [0 1 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l2 [0 2 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l2 [0 3 0]) BlockType/Air)))

    ;; build east/west
    (doseq [zz (range (* -1 length) length)]
      (let [l1 (loc+ start-loc {:z zz})
            l2 (loc+ l1        {:x 1})]
        (.println System/out (format "laying down %s at %s and %s" pavement-type (loc->vec l1) (loc->vec l2)))
        (.setBlockAt world l1 pavement-type)
        #_(.setBlockAt world (loc+ l1 [0 1 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l1 [0 2 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l1 [0 3 0]) BlockType/Air)
        
        (.setBlockAt world l2 pavement-type)
        #_(.setBlockAt world (loc+ l2 [0 1 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l2 [0 2 0]) BlockType/Air)
        #_(.setBlockAt world (loc+ l2 [0 3 0]) BlockType/Air)))))


(defn front-and-center! []
  (doseq [name (player-names)]
    (teleport-to-ground-height name [0 0 0])))

(comment
  (->location-tuple (player-location "kyle_burton"))
  ;; this is a flat area in the default generated world
  (teleport-to "kyle_burton" [-119 82 -421])

  (teleport-to "kyle_burton" (loc+ (player-location "kyle_burton") [0 10 0]))


  (bring-em-all-here!
   (loc+
    (player-location "kyle_burton")
    [20 5 0]))
  (light-em-all-up!)

  (light-em-up!)


  (front-and-center!)

  (build-road-system
   (player-location "kyle_burton")
   32
   {:block-type BlockType/Stone})

  (-> [0 0 0]
      ->location
      loc-set-at-ground-height!)

  (teleport-to-ground-height "kyle_burton" [0 0 0])

  (loc->ground-height (player-location "kyle_burton"))

  (after
   2000
   (mob-shooter "kyle_burton"))

  (->
   "kyle_burton"
   ->player
   (.teleportTo (Location. 0 64 0)))

  (->
   "kyle_burton"
   ->player
   player-location
   .getWorld)



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
       (.setY loc (+ (.getY loc) 10))
       (.setZ loc (+ (.getZ loc) 10))
       (doseq [mob (all-mobs)]
         (Thread/sleep 50)
         (.teleportTo mob loc)
         (.setFireTicks mob 600))
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

  net.canarymod.api.entity.living.humanoid.Player


  (net.canarymod.Canary/getServer)


  )
