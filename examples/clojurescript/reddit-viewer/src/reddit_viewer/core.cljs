(ns reddit-viewer.core
  (:require
   [ajax.core :as ajax]
   [reagent.core :as r]))

;; -------------------------
;; our data
(defonce posts (r/atom nil))
(defonce last-request (r/atom nil))

;; from the Chrome javascript console, you can view the atoms by
;; calling the cljs functions directly:
;; 
;; cljs.core.clj__GT_js(cljs.core.deref(reddit_viewer.core.last_request))

(defn find-posts-with-preview [posts]
  (filter #(= (:post_hint %) "image") posts))

(defn load-posts []
  (ajax/GET "https://www.reddit.com/r/Catloaf.json?sort=new&limit=10"
            {:handler         #(do
                                 (reset! last-request %)
                                 (->> (get-in % [:data :children])
                                      (map :data)
                                      (find-posts-with-preview)
                                      (reset! posts)))
             :response-format :json
             :keywords?       true}))

;; -------------------------
;; Views

(defn display-post [{:keys [permalink subreddit title score url]}]
  [:div.card.m-2
   [:div.card-block
    [:h4.card-title
     [:a {:href (str "http://reddit.com" permalink)} title " "]]
    [:div [:span.badge.badge-info {:color "info"} subreddit " score " score]]
    [:img {:width "300px" :src url}]]])

(defn home-page []
  [:div [:h2 "Welcome to Reagent"]
   [display-post (first @posts)]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
