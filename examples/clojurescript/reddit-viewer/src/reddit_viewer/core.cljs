(ns reddit-viewer.core
  (:require
   [ajax.core           :as ajax]
   [reagent.core        :as r]
   [reddit-viewer.chart :as chart]))

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
(defn navitem [title view id]
  [:li.nav-item
   {:class-name (when (= id @view) "active")}
   [:a.nav-link
    {:href     "#"
     :on-click #(reset! view id)}
    title]])

(defn navbar [view]
  [:nav.navbar.navbar-toggleable-md.navbar-light.bg-faded
   [:ul.navbar-nav.mr-auto.nav
    {:className "navbar-nav mr-auto"}
    [navitem "Posts" view :posts]
    [navitem "Chart" view :chart]]])

(defn sort-posts [title sort-key]
  (when-not (empty? @posts)
    [:button.btn.btn-secondary
     ;; {:on-click #(swap! posts (partial sort-by sort-key))}
     {:on-click #(swap! posts (fn [posts] (reverse (sort-by sort-key posts))))}
     (str "sort posts by " title)]))

(defn display-post [{:keys [permalink subreddit title score url]}]
  [:div.card.m-2
   [:div.card-block
    [:h4.card-title
     [:a {:href (str "http://reddit.com" permalink)} title " "]]
    [:div [:span.badge.badge-info {:color "info"} subreddit " score " score]]
    [:img {:width "300px" :src url}]]])

(defn display-posts [posts]
  (when-not (empty? posts)
    [:div
     (for [posts-row (partition-all 3 posts)]
       ^{:key posts-row}
       [:div.row
        (for [post posts-row]
          ^{:key post}
          [:div.col-4 [display-post post]])])]))

(defn home-page []
  (let [navbar-state (r/atom :posts)]
    (fn []
      [:div
       [navbar navbar-state]
       [:div.card>div.card-block
        [:div.btn-group
         [sort-posts "score" :score]
         [sort-posts "comments" :num_comments]]
        (case @navbar-state
          :chart [chart/chart-posts-by-votes posts]
          :posts [display-posts @posts])]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
