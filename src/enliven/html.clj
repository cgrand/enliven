(ns enliven.html
  (:require [enliven.core.actions :as action]
    [enliven.core.segments :as seg]
    [enliven.core.locs :as loc]
    [enliven.core.selectors :as sel]
    [enliven.core.grounder :as grounder]
    [enliven.core.plans :as plan]
    [enliven.html.emit.static :as static]
    [enliven.commons.emit.static :refer [render tighten]]
    [enliven.html.css-selectors :as css]
    [clojure.string :as str]))

;; html-specific segments
(seg/defsegment classes [class-attr classes] 
  :fetch 
    (zipmap (re-seq #"\S+" (or class-attr "")) (repeat true))
  :putback 
    (some->> classes (keep (fn [[k v]] (when v k))) seq (str/join " ")))

(defn sel [selector]
  (cond
    (string? selector) (css/css selector)
    (keyword? selector) (css/css (name selector))
    :else selector))

;; html-specific transformations
(defn class
  "Set a class (on the selected elements) when the value at the corresponding path in the model is true."
  {:arglists '([selector class path & class+paths])}
  [selector & class+paths]
  (let [selector (sel selector)]
    (for [[class path] (partition 2 class+paths)]
      [(sel/chain selector (sel/by-path [:attrs :class classes (name class)]))
       (action/replace path)])))

(defn attr 
  "Set an attribute (on the selected elements) to the value at the corresponding path in the model."
  {:arglists '([selector attr path & attr+paths])}
  [selector & attr+paths]
  (let [selector (sel selector)]
    (for [[attr path] (partition 2 attr+paths)]
      [(sel/chain selector (sel/by-path [:attrs (keyword attr)]))
       (action/replace path)])))

(defn content
  "Set the content (of the selected elements) the value at the path in the model."
  [selector path]
  (let [selector (sel selector)]
    [[(sel/chain selector (sel/by-path [:content (seg/slice 0 java.lang.Long/MAX_VALUE)])) (action/replace path)]]))

(defn prepend [selector path]
  (let [selector (sel selector)]
    [[(sel/chain selector (sel/by-path [:content (seg/slice 0 0)])) (action/replace path)]]))

(defn append [selector path]
  (let [selector (sel selector)]
    [[(sel/chain selector (sel/by-path [:content (seg/slice java.lang.Long/MAX_VALUE java.lang.Long/MAX_VALUE)])) (action/replace path)]]))

(defn dup 
  "Dup[licate] the selected nodes for each item in the collection at the path in the model.
  Each copy is then transformed using the specified transformations.
  For these transformatons the model is restricted to the item."
  [selector path & transformations]
  (let [selector (sel selector)]
    [[selector (action/dup path (reduce into #{} transformations))]]))

#_(defn if [selector test then else]
   [[selector [::action/if 0 [test] then else]]])

#_(defn discard [selector]
   [[selector action/discard]])

;; crude
(defn template [src & transformations]
  (let [node src
        plan (-> (reduce into #{} transformations)
               (grounder/ground node) plan/plan)
        emitted (->> plan (static/emit node) tighten)]
    (fn [data] (render emitted data))))
