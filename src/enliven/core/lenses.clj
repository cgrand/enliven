(ns enliven.core.lenses)

(defprotocol Lens
  (-fetch [lens value])
  (-putback [lens value subvalue])
  (-reduce [lens f init])
  (-form [lens]))

(defn fetch [value l]
  (-fetch l value))

(defn putback [value l subvalue]
  (-putback l value subvalue))

(defn update [node l f & args]
  (-putback l node (apply f (-fetch l node) args)))


(doseq [type [clojure.lang.Keyword clojure.lang.Symbol String]]
  (derive type ::associative)
  (extend type
    Lens {:-fetch #(get %2 %1)
          :-putback #(assoc %2 %1 %3)
          :-reduce (fn [lens f init] (f init lens))
          :-form identity} ))

(derive Number ::associative)
(derive Number ::ordered)
(extend Number
    Lens {:-fetch #(nth %2 %1)
          :-putback #(assoc %2 %1 %3)
          :-reduce (fn [lens f init] (f init lens))
          :-form identity} )

(extend java.util.regex.Pattern
  Lens
  {:-fetch (comp vec re-seq)
   :-putback (fn [p s replacements]
               (let [m (re-matcher p s)]
                 (loop [sb (StringBuilder.) i 0 [s' & ss] replacements]
                   (if (.find m)
                     (recur (-> sb (.append (subs s i (.start m))) (.append (str s')))
                       (.end m) ss)
                     (-> sb (.append (subs s i)) .toString)))))
   :-reduce (fn [lens f init] (f init lens))
   :-form identity})

(defn- bound [mn n mx]
  (-> n (max mn) (min mx)))

(defn- spliceable? [x]
  (or (nil? x) (sequential? x)))

(defrecord Slice [from to]
  Lens
  (-fetch [lens x]
    (let [n (count x)]
      (subvec x (bound 0 from n) (bound 0 to n))))
  (-putback [lens x v]
    (let [n (count x)]
      (-> x
        (subvec 0 (bound 0 from n))
        (into (if (spliceable? v) v (list v)))
        (into (subvec x (bound 0 to n) n)))))
  (-reduce [lens f init] (f init lens))
  (-form [lens] (list `slice from to)))

(derive Slice ::ordered)
(derive ::slice ::ordered)

(defn slice [from to] (Slice. from to))

(defn slice? [x] (instance? Slice x))

(defn- collapse-slices [rf]
  (let [prev-slice (volatile! nil)]
    (fn
      ([] (rf))
      ([s] (if-some [slice @prev-slice]
             (rf (rf s slice))
             (rf s )))
      ([s lens]
        (if-some [slice @prev-slice]
          (if (slice? lens)
            (let [pfrom (:from slice) pto (:to slice)]
              (vreset! prev-slice (slice (bound pfrom (+ pfrom (:from lens)) pto) (bound pfrom (+ pfrom (:to lens)) pto)))
              s)
            (let [s (rf s slice)]
              (vreset! prev-slice nil)
              (if (reduced? s)
                s
                (rf s lens))))
          (if (slice? lens)
            (do (vreset! prev-slice lens) s)
            (rf s lens)))))))

(defn lenses [l]
  (let [f (collapse-slices conj)]
    (f (-reduce l f []))))

(defn- simple-lens-type 
  "Type of a non-composite lens." [l]
  (let [form (-form l)]
    (if (sequential? form)
      (keyword (first form))
      (class form))))

(defmulti disjoint? (fn [a b] [(simple-lens-type a) (simple-lens-type b)]))

(defmethod disjoint? :default [a b] false)

(defmethod disjoint? [::associative ::ordered] [a b]
  true)

(defmethod disjoint? [::ordered ::associative] [a b]
  true)

(defmethod disjoint? [::associative ::associative] [a b]
  (not= a b))

(defmethod disjoint? [::ordered ::ordered] [a b]
  (not= a b))

(doseq [v [::associative ::ordered] w [::associative ::ordered]]
  (prefer-method disjoint? [::ordered ::ordered] [v w])
  (prefer-method disjoint? [v w] [::associative ::associative]))


(defmethod disjoint? [Number ::slice] [n slice]
  (or (< n (:from slice)) (<= (:to slice) n)))

(defmethod disjoint? [::slice Number] [slice n]
  (or (< n (:from slice)) (<= (:to slice) n)))

(defmethod disjoint? [::slice ::slice] [a b]
  (or (<= (:to a) (:from b)) (<= (:to b) (:from a))))

(defn gcl 
  "Returns nil when it can't be determined a and b don't conflict. Returns [common rem-a rem-b] otherwise."
  [a b]
  (loop [common [] as (lenses a) bs (lenses b)]
    (if-some [[a & as :as ra] (seq as)]
      (if-some [[b & bs :as rb] (seq bs)]
        (cond
          (= a b) (recur (conj common a) as bs)
          (disjoint? a b) [common ra rb]
          :else nil)
        [common (vec ra) []])
      [common [] (vec bs)])))

(def sequential-lens-impl
  {:-fetch (fn [lenses x] (reduce fetch x lenses))
   :-putback (fn [lenses value subvalue]
               (letfn [(pb [lenses value]
                         (if-let [[lens & lenses] (seq lenses)]
                           (-putback lens value
                             (pb lenses (-fetch lens value)))
                           subvalue))]
                 (pb lenses value)))
   :-reduce (fn [lenses f init]
              (reduce (fn [s lens] (-reduce lens f s)) init lenses))
   :-form lenses})

(doseq [type [nil clojure.lang.APersistentVector clojure.lang.ASeq clojure.lang.LazySeq]]
  (extend type Lens sequential-lens-impl))

