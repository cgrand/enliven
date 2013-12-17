(ns enliven.core.segments)

(defn- bound [mn n mx]
  (-> n (max mn) (min mx)))

(defn- slice-fetch [x [from to]]
  (let [n (count x)]
    (subvec x (bound 0 from n) (bound 0 to n))))

(defn- slice-putback [x [from to] v]
  (let [n (count x)]
    (-> x 
      (subvec 0 (bound 0 from n))
      (into (if (sequential? v) v (list v)))
      (into (subvec x (bound 0 to n) n)))))

(def ^:private kw-fetches {})
(def ^:private kw-putbacks {})

(defn seg-class [seg]
  (cond
    (keyword? seg) :keyword
    (vector? seg) :range
    (number? seg) :number
    (string? seg) :string
    :else (throw (ex-info "Unsupported segment type" {:seg seg}))))

(defn bounds [seg v]
  (let [[from to] (case (seg-class seg)
                    :range seg
                    :number [seg (inc seg)])
        n (count v)]
    [(bound 0 from n) (bound 0 to n)]))

(defn fetch [node seg]
  (case (seg-class seg)
    :keyword (let [get (kw-fetches seg get)] (get node seg)) ; TODO allow extension
    :range (slice-fetch node seg)
    :number (nth node seg)
    :string (get node seg)))

(defn putback [node seg v]
  (case (seg-class seg)
    :keyword (let [assoc (kw-putbacks seg assoc)] (assoc node seg v)) ; TODO allow extension
    :range (slice-putback node seg v)
    :number (assoc node seg v)
    :string (assoc node seg v)))

(defn special-keyword? [keyword]
  (or (contains? kw-fetches keyword) (contains? kw-putbacks keyword)))

(defn update [node seg f & args]
  (putback node seg (apply f (fetch node seg) args)))

(defn defseg [kw fetch putback]
  (when-not (namespace kw)
    (throw (ex-info "Can't specialize fetch/putback for non-namespaced keywords." {:kw kw})))
  (alter-var-root #'kw-fetches assoc kw fetch)
  (alter-var-root #'kw-putbacks assoc kw putback))

(defn cmp 
  "Compare segments. Compared ranges must be disjoint or equal." 
  [a b]
  (let [ta (seg-class a)
        tb (seg-class b)]
    (cond
      (not= ta tb) (compare ta tb)
      (not= ta :vector) (compare a b)
      (= a b) 0
      (<= (nth a 1) (nth b 0)) -1
      :else +1)))