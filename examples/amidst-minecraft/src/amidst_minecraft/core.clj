(ns amidst-minecraft.core
  (:require
   [nrepl.server          :refer [start-server start-server]]
   [cider.nrepl           :refer [cider-nrepl-handler]]
   [clojure.tools.logging :as log]
   [clojure.pprint        :as pprint]))

(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4046}}))


(defn -main [& args]
  (.setLevel (org.slf4j.LoggerFactory/getLogger org.slf4j.Logger/ROOT_LOGGER_NAME) ch.qos.logback.classic.Level/INFO)
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "amidst-minecraft.core/main: nrepl started on port=%s" (-> @config :nrepl :port))
  (log/infof "amidst-minecraft.core/main: args=%s" args))


(comment

  (def world-seed (amidst.mojangapi.world.WorldSeed/fromSaveGame (Long/parseLong "4351210063826852167")))

  world-seed

  (let [world-seed       (amidst.mojangapi.world.WorldSeed/fromSaveGame (Long/parseLong "4351210063826852167"))
        world-type       amidst.mojangapi.world.WorldType/DEFAULT
        world-options    (amidst.mojangapi.world.WorldOptions. world-seed world-type)
        version-features (amidst.mojangapi.world.versionfeatures.DefaultVersionFeatures/builder nil nil)]
    world-options
    (amidst.mojangapi.world.World.
     ;; worldOptions
     world-options
     ;; movablePlayerList
     (amidst.mojangapi.world.player.MovablePlayerList/dummy)
     ;; recognisedVersion
     amidst.mojangapi.minecraftinterface.RecognisedVersion/_1_17
     ;; biomeList
     amidst.mojangapi.world.versionfeatures.DefaultBiomes/DEFAULT_BIOMES
     ;; enabledLayers
     (java.util.ArrayList.)
     ;; overworldBiomeDataOracle

     ;; netherBiomeDataOracle
     (java.util.Optional/empty)
     ;; endIslandOracle
     ;; slimeChunkOracle
     ;; spawnProducer
     ;; strongholdProducer
     ;; playerProducer
     ;; villageProducer
     ;; templeProducer
     ;; mineshaftProducer
     ;; oceanMonumentProducer
     ;; woodlandMansionProducer
     ;; oceanFeaturesProducer
     ;; netherFortressProducer
     ;; endCityProducer



     ))


  (let [args                    (into-array String ["-seed" "4351210063826852167"])
        ;; args                    (into-array String ["-mcpath" "/home/kyle/minecraft/minecraft/2020.linode-shared/"])
        ;; args                    (into-array String ["-mcpath" "/home/kyle/minecraft/minecraft/2020.linode-shared"])
        command-line-parameters (amidst.CommandLineParameters.)
        metadata                (amidst.Amidst/createMetadata)
        parser                  (org.kohsuke.args4j.CmdLineParser. command-line-parameters
                                                                   (.. (org.kohsuke.args4j.ParserProperties/defaults)
                                                                       (withShowDefaults false)
                                                                       (withUsageWidth 120)
                                                                       (withOptionSorter nil)))]
    ;; (.getDeclaredMethods (.getClass amidst.Amidst))
    ;; (.getDeclaredMethods amidst.Amidst)
    (let [method (->>
                  (.getDeclaredMethods amidst.Amidst)
                  (filter #(= "run" (.getName %)))
                  first)]
      (.setAccessible method true)
      (.parseArgument parser args)
      ;; (amidst.Amidst/run command-line-parameters metadata parser)
      (.invoke method nil (into-array Object [command-line-parameters metadata parser]))))

  )
