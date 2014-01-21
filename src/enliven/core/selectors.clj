(ns enliven.core.selectors
  (:require [enliven.core.locs :as locs]
    [enliven.core.segments :as seg]))

;; A selector is a function from one loc to a coll of locs; loc -> locs 
(defn locs [root sel]
  (sel (locs/loc root)))

(defn paths [root sel]
  (map locs/path (locs root sel)))

(defn nodes [root sel]
  (map locs/node (locs root sel)))

(defn chain
  "Composes several selectors, from left to right."
  ([] list)
  ([sel] sel)
  ([sela selb]
    (fn [loc]
      (for [loc (sela loc), loc (selb loc)]
        loc)))
  ([sela selb & sels]
    (reduce chain (chain sela selb) sels)))

(defn union
  "Returns the union selector of the specified selectors."
  [& sels]
  (fn [loc]
    (distinct (mapcat #(% loc) sels))))

(defn rights
  "A selector that returns all the rights siblings of a node
  (ordered from closest to farthest)."
  [loc]
  (let [loc (locs/canonicalize-path loc)]
    (when-let [loc (and (zero? (locs/segment loc)) (locs/up loc))]
      (let [[idx] (seg/bounds (locs/segment loc))
            loc (locs/up loc)
            n (-> loc locs/node count)]
        (map #(locs/down loc %) (range (inc idx) n))))))

(defn loc-pred 
  "Returns a filtering selector which keeps only locs for which pred is true."
  [pred]
  (fn [loc]
    (when (pred loc) (list loc))))

(defn node-pred [pred]
  (loc-pred (comp pred locs/node)))

(defn by-path [path]
  (fn [loc]
    (list (reduce locs/down loc path))))
