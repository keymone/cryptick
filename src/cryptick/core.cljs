(ns cryptick.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST]]
            [clojure.string]))

(enable-console-print!)

(defonce app-state (atom {:ticker-data [] :top-n 10 :index-data {10 {}}}))

(defonce load-ticker-data
  (GET "https://api.coinmarketcap.com/v1/ticker/"
    {:params {:limit 50} :handler #(swap! app-state assoc :ticker-data %)}))

(defn fmt-number [n] (.toLocaleString (js/Number. n)))

(defn xxx->usd [item amount]
  (* amount (js/parseFloat (item "price_usd"))))

(defn usd->xxx [item amount]
  (/ amount (js/parseFloat (item "price_usd"))))

(defn index-optimal [item total-cap total-usd index-data]
  (let [item-cap (js/parseFloat (item "market_cap_usd"))
        item-frac (/ item-cap total-cap)]
    (usd->xxx item (* total-usd item-frac))))

(defn index-item [item total-cap value set-value optimal]
  (let [item-cap (js/parseFloat (item "market_cap_usd"))
        item-pct (/ (.round js/Math (/ item-cap total-cap 0.0001)) 100)]
    [:tr {:key (item "id")}
      [:td.col-sm-2 (item "name")]
      [:td.col-sm-2.text-right "$ " (fmt-number item-cap)]
      [:td.col-sm-1.text-right (fmt-number item-pct) " %"]
      [:td.col-sm-1 [:input.form-control.input-sm.small
                     {:type "text"
                      :default-value (str value)
                      :on-change set-value}]]
      [:td.col-sm-2.text-right
        (if-not (= 0 optimal)
          [:span (fmt-number optimal) " "
                 [:abbr.initialism {:title (item "name")} (item "symbol")]])]
      [:td.col-sm-4
        (if-not (= 0 optimal)
          (if (> value optimal)
            [:span [:span.caret.caret-red] " " (fmt-number (- value optimal))]
            [:span.dropup [:span.caret.caret-green] " " (fmt-number (- optimal value))]))]]))

(defn ticker-by-id [id ticker-data]
  (first (filter #(= id (% "id")) ticker-data)))

(defn index-view [{id "id" :as item}
                  total-cap total-usd
                  {{value :value} id :as index-data}
                  index-path]
  (index-item item total-cap value
    #(swap! app-state assoc-in (concat index-path [id :value]) (-> % .-target .-value))
    (index-optimal item total-cap total-usd index-data)))

(defn index-content [ticker-data index-data index-path]
  (let [total-cap (reduce + (map #(js/parseFloat (% "market_cap_usd"))
                                 ticker-data))
        total-usd (reduce + (map (fn [[id {val :value}]]
                                   (xxx->usd (ticker-by-id id ticker-data) val))
                                 index-data))]
    (list
      [:div.row {:key "total-cap"}
        [:div.col-sm-3 [:h4 "Total market cap:"]]
        [:div.col-sm-3 [:h4 "$ " (fmt-number total-cap)]]]
      [:div.row {:key "total-usd"}
        [:div.col-sm-3 [:h4 "Profile value:"]]
        [:div.col-sm-3 [:h4 "$ " (fmt-number total-usd)]]]
      [:table.table.table-stripped.table-condensed.form-group.form-group-sm
        {:key "index-view"}
        [:tbody
          [:tr [:td.col-sm-2 "Name"]
               [:td.col-sm-2.text-right "Market cap"]
               [:td.col-sm-1.text-right "Market %"]
               [:td.col-sm-1 "Profile value"]
               [:td.col-sm-2.text-right "Optimal value"]
               [:td.col-sm-4
                 "Optimal action ("
                 [:label.small {:for "account_fees"} "fees"] " "
                 [:input {:type "checkbox" :id "account_fees"}] " "
                 [:label.small {:for "account_volume"} "volume"] " "
                 [:input {:type "checkbox" :id "account_fees"}]
                 ")"]]
          (map #(index-view % total-cap total-usd index-data index-path)
               ticker-data)]])))

(defn top-n-index []
  (let [{:keys [ticker-data index-data top-n]} @app-state]
    [:div.container
      [:div.row {:key "header"}
        [:div.col-sm-12
          [:h1
            "Crypto Index "
            [:input#top-n
             {:default-value top-n
              :on-change
              #(let [v (int (-> % .-target .-value))]
                (swap! app-state assoc-in [:index-data v] {})
                (swap! app-state assoc :top-n v))}]]]]

      (index-content
        (take top-n ticker-data) (index-data top-n) [:index-data top-n])]))

(reagent/render-component [top-n-index] (. js/document (getElementById "app")))

(defn on-js-reload [])
