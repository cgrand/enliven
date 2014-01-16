(ns enliven.core.paths
  (:require [enliven.core.segments :as seg]))

;; paths are made of segments
;; segments are keywords or numbers or ranges [from to] (to exclusive)
;; n IS not equivalent to [n (inc n)], the later denote a slice and not a node.

(defn canonical 
  "Canonicalize a path: collapse unneeded range segments, add a singleton range for each direct access." 
  [segs]
  (loop [range-mode false segs segs path []]
    (if-let [[seg & segs] (seq segs)]
      (if range-mode
        (if (seg/slice? seg)
          (let [[pfrom] (peek path)
                [from to] (seg/bounds seg)]
            (recur true segs (-> path pop (conj (seg/slice (+ pfrom from) (+ pfrom to))))))
          (recur false segs (conj path seg)))
        (recur (seg/slice? seg) segs
          (if (number? seg)
            (conj path (seg/slice seg (inc seg)) 0)
            (conj path seg))))
      path)))

(defn path [path-or-seg]
  ())

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

(defn fetch-in [x path]
  (if (sequential? path)
    (reduce seg/fetch x path)
    (seg/fetch x path)))

(defn fetcher-in [path]
  (if (sequential? path)
    (reduce (fn
              ([] identity)
              ([f g] (comp g f)))
      (map seg/fetcher path))
    (seg/fetcher path)))
