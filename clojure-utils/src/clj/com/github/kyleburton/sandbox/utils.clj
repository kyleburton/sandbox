(ns com.github.kyleburton.sandbox.utils)

(defn- get-user-home []
  (System/getProperty "user.home"))

(defmulti expand-file-name class)

(defmethod expand-file-name String [#^String path]
  (cond (.startsWith path "~/")
        (.replaceFirst path "^~(/|$)" (str (get-user-home) "/"))
        (.startsWith path "file://~/")
        (.replaceFirst path "^file://~/" (str "file://" (get-user-home) "/"))
        true
        path))

