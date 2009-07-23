(ns com.github.kyleburton.sandbox.json
  (:import [net.sf.json JSONObject]))

(JSONObject/fromObject {:a 1 :b 2})

(.toString :a)