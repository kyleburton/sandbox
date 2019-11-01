(ns personal-portal.web.api.v1.info
  (:require
   [personal-portal.web.router :as router :refer [defapi defget defpost]]
   [personal-portal.web.helpers :refer [http-200-json]]))

;; nb: we want routes(uris) to be able to support
;; different HTTP methods, how do we want to represent that?
;; what if we want diff middleware for different

(defget index ["/"] [request]
  (def get-request request)
  (http-200-json
   {:message "hello there"}))


(defget get-id ["/:id"] [request]
  (def get-by-id-request request)
  (http-200-json
   {:message "hello there"
    :id      (-> request :route-params :id)}))

;; (defpost keep-the-thing ::router/post ["/keep"] [request]
;;   (def post-request request)
;;   (http-200-json
;;    {:message "I gotcha!"
;;     :echo    "...put your body here..."}))
