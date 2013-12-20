(ns enliven.html
  (:require [enliven.core.actions :as action]
    [enliven.core.segments :as seg]
    [enliven.core.locs :as loc]
    [enliven.core.selectors :as sel]
    [clojure.string :as str]))

;; html-specific segments
(seg/defsegment classes [class-attr classes] 
  :fetch 
    (zipmap (re-seq #"\S+" (or class-attr "")) (repeat true))
  :putback 
    (some->> classes (keep (fn [[k v]] (when v k))) seq (str/join " ")))

;; html-specific transformations
(defn class
  "Set a class (on the selected elements) when the value at the corresponding path in the model is true."
  {:arglists '([selector class path & class+paths])}
  [selector & class+paths]
  (for [[class path] (partition 2 class+paths)]
    [(sel/chain selector (sel/by-path [:attrs :class classes (name class)]))
     (action/replace path)]))

(defn attr 
  "Set an attribute (on the selected elements) to the value at the corresponding path in the model."
  {:arglists '([selector attr path & attr+paths])}
  [selector & attr+paths]
  (for [[attr path] (partition 2 attr+paths)]
    [(sel/chain selector (sel/by-path [:attrs (keyword attr)]))
     (action/replace path)]))

(defn content
  "Set the content (of the selected elements) the value at the path in the model."
  [selector path]
  [[(sel/chain selector (sel/by-path [:content [0 java.lang.Long/MAX_VALUE]])) (action/replace path)]])

(defn prepend [selector path]
  [[(sel/chain selector (sel/by-path [:content [0 0]])) (action/replace path)]])

(defn append [selector path]
  [[(sel/chain selector (sel/by-path [:content [java.lang.Long/MAX_VALUE java.lang.Long/MAX_VALUE]])) (action/replace path)]])

(defn dup 
  "Dup[licate] the selected nodes for each item in the collection at the path in the model.
  Each copy is then transformed using the specified transformations.
  For these transformatons the model is restricted to the item."
  [selector path & transformations]
  [[selector (action/dup path (reduce into #{} transformations))]])

(defn if [selector test then else]
  [[selector [::action/if 0 [test] then else]]])

(defn discard [selector]
  [[selector action/discard]])
