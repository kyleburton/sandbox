(ns com.github.kyleburton.sandbox.yaml
  (:import [org.ho.yaml Yaml]))

;; symbol's don't dump?
;; (Yaml/dump {:a 1 :b 2 :c [3 4 5]})

(println
 (Yaml/dump
  (doto (java.util.HashMap.) (.put "a" 1)
        (.put "b" 2)
        (.put "c" [3 4 5]))))

;; quite nice, but can we round-trip?
;; --- 
;; b: 2
;; c: !clojure.lang.LazilyPersistentVector
;;   - 3
;;   - 4
;;   - 5
;; a: 1


;; NO :(
;; (Yaml/load
;;  (Yaml/dump
;;   (doto (java.util.HashMap.) (.put "a" 1)
;;         (.put "b" 2)
;;         (.put "c" [3 4 5]))))

;; pure java looks ok:
(println
 (Yaml/dump
  (doto (java.util.HashMap.) (.put "a" 1)
        (.put "b" 2)
        (.put "c" (into-array [3 4 5]))))

(print {:a 1 :b 2 :c [1 2 3 "foo"]})

;; and...
(Yaml/load
 (Yaml/dump
  (doto (java.util.HashMap.) (.put "a" 1)
        (.put "b" 2)
        (.put "c" (into-array [3 4 5]))))))

;; ...we can read it back, good


;; so, how do we teach Yaml to work with Clojure data types?

(bean (org.ho.yaml.YamlConfig/getDefaultConfig))

;;{:decodingAccessScope #<HashMap {field=public, property=public, constructor=public}>, :suppressWarnings false, :class org.ho.yaml.YamlConfig, :dateFormat nil, :dateFormatter nil, :handlers #<HashMap {java.math.BigInteger=[class java.math.BigInteger], java.awt.Color=org.ho.yaml.wrapper.ColorWrapper, java.awt.Point=org.ho.yaml.wrapper.PointWrapper, java.lang.Class=org.ho.yaml.wrapper.ClassWrapper, java.io.File=[class java.io.File], java.math.BigDecimal=[class java.math.BigDecimal], java.util.Date=[class java.util.Date], java.awt.Dimension=org.ho.yaml.wrapper.DimensionWrapper}>, :transfers nil, :minimalOutput false, :encoding "UTF-8", :encodingAccessScope #<HashMap {field=public, property=public}>, :indentAmount "  "}