(ns rk
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [zenform.model :as zm]))

(defn checkbox [{:keys [checked]}]
  (fn [{:keys [checked]}]
    [:div.checkbox {:class (if checked :checked :unchecked)}
     [:span.checkmark]]))

(defn dropdown [{:keys [on-hide dropdown item]}]
  (r/with-let [show-dropdown? (r/atom nil)
               dropdown-ref   (r/atom nil)
               on-click       (r/atom nil)
               hide-dropdown  (fn []
                                (.. js/document -body
                                    (removeEventListener "click" @on-click))
                                (when on-hide
                                  (on-hide))
                                (reset! show-dropdown? false))
               show-dropdown  (fn []
                                (.. js/document -body
                                    (addEventListener
                                     "click"
                                     (reset! on-click
                                             (fn [ev]
                                               (when (and @dropdown-ref
                                                          (not (.. @dropdown-ref (contains (.. ev -target)))))
                                                 (hide-dropdown))))))
                                (reset! show-dropdown? true))
               props {:hide-dropdown hide-dropdown
                      :show-dropdown show-dropdown}]
    (if @show-dropdown?
      [:div.dropdown {:ref #(reset! dropdown-ref %)}
       (dropdown props)]
      (item props))))

(defn zselect [form-path path & _]
  (let [node       (rf/subscribe [:zf/node form-path path])
        find-value (fn [{:keys [value items]}]
                     (first (filter #(= (:value %) value) items)))]
    (fn [& _]
      (let [node @node]
        (if node
          [dropdown {:dropdown (fn [{:keys [hide-dropdown]}]
                                 [:<>
                                  [:div.input]
                                  [:div.item-list
                                   (for [{:keys [value label]} (:items node)]
                                     ^{:key (or value 0)}
                                     [:div.item {:class (str/join " "
                                                                  [(when (= (:value node) value)
                                                                     "active")
                                                                   (when-not value
                                                                     "empty")])
                                                 :on-click (fn []
                                                             (hide-dropdown)
                                                             (rf/dispatch [:zf/set-value form-path path value]))}
                                      label])]])
                     :item (fn [{:keys [show-dropdown hide-dropdown]}]
                             [:div.select.value {:on-click show-dropdown}
                              [:span (or (:label (find-value node)) "")]
                              [:img {:src "image/arrow.svg"}]])}]
          [:div "loading..."])))))

(defn str-includes? [s substr]
  (str/includes? (str/lower-case s) substr))

(defn find-value-index [value node]
  (reduce
   (fn [_ [idx v]]
     (when (= (:value v) value)
       (reduced idx)))
   nil (:value node)))

(defn multiselect [form-path path & _]
  (let [node          (rf/subscribe [:zf/node form-path path])
        filter-string (r/atom "")]
    (fn [& _]
      (if-let [node @node]
        (let [node-value (zm/get-value node)
              value-set  (set node-value)
              items      (:items node)]
          [dropdown {:dropdown
                     (fn [{:keys [hide-dropdown]}]
                       [:<>
                        [:input.multiselect {:on-change #(reset! filter-string (.. % -target -value))
                                             :auto-focus true}]
                        [:div.item-list
                         (let [group (group-by (comp boolean value-set :value) items)
                               items (concat (get group true) (get group false))
                               filter-string (str/lower-case @filter-string)
                               items (filter #(str-includes? (:label %) filter-string) items)]
                           (for [{:keys [value label]} items]
                             (let [exists? (first (filter #(= % value) node-value))]
                               ^{:key (or value 0)}
                               [:div.multi.item
                                {:on-click
                                 (fn []
                                   (let [idx (when exists?
                                               (find-value-index value node))]
                                     (if exists?
                                       (rf/dispatch [:zf/remove-collection-item form-path path idx])
                                       (rf/dispatch [:zf/add-collection-item form-path path value]))))}
                                [:div.item-block
                                 [checkbox {:checked (boolean exists?)}]
                                 [:span label]]])))]])

                     :item
                     (fn [{:keys [show-dropdown hide-dropdown]}]
                       (let [labels (map :label (filter #(value-set (:value %)) items))
                             value (str/join ", " labels)]
                         [:div.multiselect.value {:on-click show-dropdown}
                          [:span value]
                          [:img {:src "image/arrow.svg"}]]))}])
        [:div "loading..."]))))
