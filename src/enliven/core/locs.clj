(ns enliven.core.locs
  (:require [enliven.core.segments :as segs]
    [enliven.core.paths :as path]))

(defrecord ^:private Loc [value seg ploc])

(defn loc [x]
  (->Loc x nil nil))

(defn down [loc seg]
  (->Loc (segs/fetch (:value loc) seg) seg loc))

(defn up [loc]
  (:ploc loc))

(defn node [loc]
  (:value loc))

(defn segment [loc]
  (:seg loc))

(defn path [loc]
  (loop [path () loc loc]
    (if-let [ploc (up loc)]
      (recur (conj path (:seg loc)) ploc)
      path)))

(defn root [loc]
  (if-let [ploc (up loc)]
    (recur ploc)
    (:value loc)))

(defn canonicalize-path [loc']
  (reduce down (-> loc' root loc) (path/canonical (path loc'))))


