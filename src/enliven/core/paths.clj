(ns enliven.core.paths
  (:require [enliven.core.segments :as seg]))

;; paths are made of segments

(defn fetch-in [x path]
  (reduce seg/fetch x path))

(defn fetch-type-in [x-type path]
  (reduce seg/fetch-type x-type path))

(defn fetcher-in [path]
  (reduce (fn
            ([] identity)
            ([f g] (comp g f)))
    (map seg/fetcher path)))

(defn simplify
  "Simplify constant paths and collapse nested slice segments.
   If it wasn't a path bu just a segment, hoist it into a path."
  [seg-or-segs]
  (reduce
    (fn [path seg]
      (let [prev-seg (peek path)]
        (cond
          (seg/const? prev-seg) [(seg/const (seg/fetch (seg/fetch nil prev-seg) seg))]
          (seg/slice? prev-seg)
          (let [[pfrom] (seg/bounds prev-seg)]
            (if (seg/slice? seg)
              (let [[from to] (seg/bounds seg)]
                ; TODO check for special bounds
                (-> path pop (conj (seg/slice (+ pfrom from) (+ pfrom to)))))
              (conj path seg)))
          (seg/const? seg) [seg]
          :else (conj path seg))))
    [] (if (or (nil? seg-or-segs)
             (sequential? seg-or-segs))
         seg-or-segs
         (list seg-or-segs))))

(defn canonical
  "Canonicalize a path: simplify and add a parent slice for each indexed access.
   And if it wasn't a path bu just a segment, hoist it into a path."
  [seg-or-segs]
  (reduce
    (fn [path seg]
      (let [prev-seg (peek path)]
        (cond
          (not (number? seg)) (conj path seg)
          (seg/slice? prev-seg)
          (let [[pfrom] (seg/bounds prev-seg)]
            (let [from (+ pfrom seg)]
              ; TODO check for special bounds
              (-> path pop (conj (seg/slice from (inc from)) 0))))
          :else (conj path (seg/slice seg (inc seg)) 0))))
    [] (simplify seg-or-segs)))

(defn minimal
  [seg-or-segs]
  (reduce
    (fn [path seg]
      (let [prev-seg (peek path)]
        (if (and (number? seg) (seg/slice? prev-seg))
          (let [[pfrom] (seg/bounds prev-seg)]
            ; TODO check for special bounds
            (-> path pop (conj (+ pfrom seg))))
          (conj path seg))))
    [] (simplify seg-or-segs)))

(defn const? [path]
  (-> path first seg/const?))