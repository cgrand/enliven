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

(defn canonical 
  "Canonicalize a path: simplify constant paths, collapse nested slice segments, add a parent slice for each indexed access.
   And if it wasn't a path bu just a segment, hoist it into a path." 
  [seg-or-segs]
  (reduce
    (fn [path seg]
      (let [prev-seg (peek path)]
        (cond
          (seg/const? prev-seg) [(seg/const (seg/fetch (seg/fetch nil prev-seg) seg))]
          (seg/slice? prev-seg)
          (let [[pfrom] (seg/bounds prev-seg)]
            (cond
              (number? seg)
              (let [from (+ pfrom seg)]
                ; TODO check for special bounds
                (-> path pop (conj (seg/slice from (inc from)) 0)))
              (seg/slice? seg)
              (let [[from to] (seg/bounds seg)]
                ; TODO check for special bounds
                (-> path pop (conj (seg/slice (+ pfrom from) (+ pfrom to)))))
             :else (conj path seg)))
          (number? seg) (conj path (seg/slice seg (inc seg)) 0)
          (seg/const? seg) [seg]
          :else (conj path seg))))
    [] (if (or (nil? seg-or-segs)
             (sequential? seg-or-segs))
         seg-or-segs
         (list seg-or-segs))))

(defn- broader-or-equal? [a b]
  (or (= a b)
    (and (seg/slice? a) ; if a is a slice then b is a slice because of the canonicalization
      (let [[fa ta] (seg/bounds a) [fb tb] (seg/bounds b)]
        (<= fa fb tb ta)))))

(defn broad-prefix? 
  "Assumes both paths are canonical, returns true if prefix is a superset of
   a prefix of path."
  [prefix path]
  (let [n (count prefix)]
    (or (zero? n)
      (and (<= n (count path))
        (= (pop prefix) (subvec path 0 (dec n)))
        (broader-or-equal? (peek prefix) (nth path (dec n)))))))

(defn remove-prefix 
  "Assumes (broad-prefix? prefix path) is true."
  [prefix path]
  (let [n (count prefix)
        a (peek prefix)
        b (nth path (dec n))]
    (into 
      (if (= a b) [] (let [[fa] (seg/bounds a) [fb tb] (seg/bounds b)]
                       [(seg/slice (- fb fa) (- tb fa))]))
      (subvec path n))))

(defn const? [path]
  (-> path first seg/const?))