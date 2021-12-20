(ns leiningen.new.kinematic-app
  (:require [leiningen.new.templates :refer [renderer name-to-path ->files
                                             project-name multi-segment
                                             sanitize-ns]]
            [leiningen.core.main :as main]))

(def render (renderer "kinematic-app"))

(defn kinematic-app
  "Creates a new default project for a kinematic web service."
  [name]
  (let [main-ns (multi-segment (sanitize-ns name))
        data {:raw-name     name
              :name         (project-name name)
              :namespace    main-ns
              :nested-dirs  (name-to-path main-ns)
              :api-port     8192
              :sanitized    (name-to-path name)}]
    (main/info "Generating fresh 'lein new' kinematic-app project.")
    (->files data
             ["README.md"                         (render "README.md"   data)]
             ["project.clj"                       (render "project.clj"   data)]
             ["src/{{sanitized}}/web.clj"         (render "web.clj"     data)]
             ["src/{{sanitized}}/session.clj"     (render "session.clj" data)]
             ["src/{{sanitized}}/helpers.clj"     (render "helpers.clj" data)]
             ["src/{{sanitized}}/api/v1/res.clj"  (render "res.clj" data)]
             ["src/{{sanitized}}/core.clj"        (render "core.clj"    data)])))
