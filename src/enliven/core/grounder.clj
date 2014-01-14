(ns enliven.core.grounder
  (:require [enliven.core.rules :as rules]
    [enliven.core.actions :as action]
    [enliven.core.selectors :as sel]
    [enliven.core.locs :as loc]
    [enliven.core.segments :as seg]))

;; a transformation is a function from loc to seq of rules
(defn- ground-transformation [transformation node]
  (transformation (loc/loc node)))

(defn ground [transformations node]
  (reduce rules/conj-rule rules/id
    (mapcat #(ground-transformation % node) transformations)))

(defn simple-transformation [selector action]
  (fn [loc]
    (for [loc (selector loc)]
      [(loc/path loc) (action/update-subs action ground-transformation (loc/node loc))])))

(defn splice-transformation [selector action]
  (fn [loc]
    (keep (fn [loc]
            (let [seg (:seg loc)
                  sloc (cond
                         (seg/slice? seg) loc
                         (number? seg) 
                         (-> loc loc/up (loc/down (seg/slice seg (inc seg)))))]
              (when sloc
                [(loc/path sloc) (action/update-subs action ground-transformation (loc/node loc))])))
      (selector loc))))

(defn composite-transformation [transformations]
  (fn [loc] (mapcat #(% loc) transformations)))

(defn at* 
  ([selector+transformations] (at* selector+transformations identity))
  ([selector+transformations sel]
    (composite-transformation
      (for [[selector t] (partition 2 selector+transformations)
            :let [selector (sel selector)]]
        (fn [loc] (mapcat t (selector loc)))))))

(defn at [& selector+transformations]
  (at* selector+transformations))
