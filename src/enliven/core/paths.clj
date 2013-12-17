(ns enliven.core.paths)

;; paths are made of segments
;; segments are keywords or numbers or ranges [from to] (to exclusive)
;; n IS not equivalent to [n (inc n)], the later denote a slice and not a node.

(defn canonical 
  "Canonicalize a path: collapse unneeded range segments, add a singleton range for each direct access." 
  [segs]
  (loop [range-mode false segs segs path []]
    (if-let [[seg & segs] (seq segs)]
      (if range-mode
        (if (vector? seg)
          (let [[pfrom] (peek path)
                [from to] seg]
            (recur true segs (-> path pop (conj [(+ pfrom from) (+ pfrom to)]))))
          (recur false segs (conj path seg)))
        (recur (vector? seg) segs 
          (if (number? seg)
            (conj path [seg (inc seg)] 0)
            (conj path seg))))
      path)))

(defn- broader-or-equal? [a b]
  (or (= a b)
    (and (vector? a) ; if a is a vector then b is a vector because of the canonicalization
      (let [[fa ta] a [fb tb] b]
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
      (if (= a b) [] (let [[fa] a [fb tb] b] [[(- fb fa) (- tb fa)]])) 
      (subvec path n))))