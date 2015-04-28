(ns enliven.core.locs
  (:require [enliven.core.lenses :as lens]))

(defrecord ^:private Loc [value seg ploc])

(defn loc [x]
  (->Loc x nil nil))

(defn down [loc seg]
  (->Loc (lens/fetch (:value loc) seg) seg loc))

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

(defn spliceable
  "Returns the immediate splicing location, if none returns nil."
  [loc]
  (let [seg (segment loc)]
    (cond
      (number? seg)
      (-> loc up (down (lens/slice seg (inc seg))))
      (lens/slice? seg) loc)))
