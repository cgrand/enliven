(ns enliven.core.paths
  (:require [enliven.core.segments :as seg]))

;; paths are made of segments

(defn fetch-in [x path]
  (reduce seg/fetch x path))

(defn fetcher-in [path]
  (reduce (fn
            ([] identity)
            ([f g] (comp g f)))
    (map seg/fetcher path)))

(defn canonical 
  "Canonicalize a path: simplify constant paths, collapse nested slice segments, add a parent slice for each indexed access.
   And if it wasn't a path bu just a segment, hoist it into a path." 
  [seg-or-segs]
  (loop [range-mode false
        segs (if (or (nil? seg-or-segs)
                   (sequential? seg-or-segs))
               seg-or-segs
               (list seg-or-segs))
        path []]
   (if-let [[seg & segs] (seq segs)]
     (cond
       (seg/const? seg)
       (let [v (fetch-in (seg/fetch nil seg) segs)]
         [(seg/const v)])
       (and (number? seg) (not (and range-mode (zero? seg))))
       (recur range-mode (list* (seg/slice seg (inc seg)) 0 segs)
         path)
       range-mode
       (if (seg/slice? seg)
         (let [[pfrom] (seg/bounds (peek path))
               [from to] (seg/bounds seg)]
           ; TODO check for special bounds
           (recur true segs (-> path pop (conj (seg/slice (+ pfrom from) (+ pfrom to))))))
         (recur false segs (conj path seg)))
       :else
       (recur (seg/slice? seg) segs (conj path seg)))
     path)))

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