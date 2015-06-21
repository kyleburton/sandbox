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

(defn loc->ground-height [loc]
  (let [loc   (->location loc)
        world (.getWorld loc)]
    (.getHighestBlockAt world (.getY loc) (.getZ loc))))

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
  (let [l1 (->location l1)]
    (Location.
     (+ (.getX l1) (:x l2 0))
     (+ (.getY l1) (:y l2 0))
     (+ (.getZ l1) (:z l2 0)))))
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
  (teleport-to "kyle_burton" (loc+ (player-location "kyle_burton") [0 10 0]))

  (player-location "kyle_burton")
  (loc+ (player-location "kyle_burton") [0 10 0])
  

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

  net.minecraft.server.MinecraftServer
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
