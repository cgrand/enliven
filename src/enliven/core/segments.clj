(ns enliven.core.segments)

(defprotocol Segment
  (-fetch [seg value])
  (-putback [seg value subvalue])
  (-expr [seg]))

(defn fetch [value seg]
  (-fetch seg value))

(defn putback [value seg subvalue]
  (-putback seg value subvalue))

(defn update [node seg f & args]
  (putback node seg (apply f (fetch node seg) args)))

(defprotocol SegmentEx
  (fetcher [seg]))

(doseq [t [clojure.lang.Keyword clojure.lang.Symbol String]]
  (extend t
    Segment {:-fetch #(get %2 %1)
             :-putback #(assoc %2 %1 %3)
             :-expr identity}))

(extend-protocol SegmentEx
  Object
  (fetcher [seg] #(-fetch seg %))
  String
  (fetcher [seg] #(get % seg)))

(doseq [t [clojure.lang.Keyword clojure.lang.Symbol]]
  (extend t
    SegmentEx {:fetcher identity}))

(defn- bound [mn n mx]
  (-> n (max mn) (min mx)))

(defn spliceable? [x]
  (or (nil? x) (sequential? x)))

(defrecord Slice [from to]
  Segment 
  (-fetch [seg x]
    (let [n (count x)]
      (subvec x (bound 0 from n) (bound 0 to n))))
  (-putback [seg x v]
    (let [n (count x)]
      (-> x 
        (subvec 0 (bound 0 from n))
        (into (if (spliceable? v) v (list v)))
        (into (subvec x (bound 0 to n) n)))))
  (-expr [seg] (list `slice from to))
  SegmentEx
  (fetcher [seg]
    (fn [x]
      (let [n (count x)]
        (subvec x (bound 0 from n) (bound 0 to n)))))
  Comparable
  (compareTo [a b]
    (cond
      (= a b) 0
      (<= (:to a) (:from b)) -1
      :else +1)))

(defn slice [from to] (Slice. from to))

(defn slice? [seg] (instance? Slice seg))

(extend Number
  Segment {:-fetch #(nth %2 %1)
           :-putback #(assoc %2 %1 %3)
           :-expr identity}
  SegmentEx {:fetcher (fn [n] #(nth % n))})

(defn seg-class [seg]
  (cond
    (slice? seg) :range
    (number? seg) :number
    :else :misc))

(defn bounds
  ([seg]
    (case (seg-class seg)
                      :range [(:from seg) (:to seg)]
                      :number [seg (inc seg)]))
  ([seg v]
    (let [[from to] (bounds seg)
          n (count v)]
      [(bound 0 from n) (bound 0 to n)])))

(defrecord Constant [v]
  Segment
  (-fetch [seg _] v)
  (-expr [seg] (list `const v))
  SegmentEx
  (fetcher [seg]
    (constantly v)))

(defn const [v] (Constant. v))

(defn const? [seg] (instance? Constant seg))

(defmacro defsegment [name doc? initial-args & methods]
  (let [[name initial-args methods] (if (string? doc?)
                                      [(vary-meta name assoc :doc doc?) initial-args methods]
                                      [name doc? (cons initial-args methods)])
        [value-arg subvalue-arg & args] initial-args
        fqname (symbol (clojure.core/name (ns-name *ns*)) 
                 (clojure.core/name name))
        auto-segment (empty? args)
        [args [_ & rest-arg]] (split-with #(not= (clojure.core/name %) "&") args)
        plain-args (take (count args) (repeatedly gensym))
        plain-rest-arg (when rest-arg (gensym))
        fetch-expr (some (fn [[name expr]] (case name :fetch expr nil)) (partition 2 methods))
        methods (for [[name expr] (partition 2 methods)]
                  (case name
                    :fetch `(-fetch [_# ~value-arg] ~expr)
                    :putback `(-putback [_# ~value-arg ~subvalue-arg] ~expr)))
        f `(fn [~@plain-args ~@(when plain-rest-arg `[& ~plain-rest-arg])]
             (let [~@(interleave args plain-args)
                   ~@(when plain-rest-arg [rest-arg plain-rest-arg])]
               (reify
                 Segment 
                 ~@methods
                 (-expr [_#] ~(if auto-segment
                                `'~fqname
                                `(list* '~fqname ~@plain-args ~plain-rest-arg)))
                 SegmentEx
                 (fetcher [_#]
                   (fn [~value-arg] ~fetch-expr))
                 Object
                 (equals [this# that#]
                   (and (satisfies? Segment that#)
                     (= (-expr this#) (-expr that#))))
                 (hashCode [this#] (hash (-expr this#)))
                 (toString [this#] (pr-str (-expr this#))))))]
    `(def ~name ~(if auto-segment (list f) f))))

(defsegment append-on-assoc ; better name wanted
  "Presents a sequence of key-value pairs (an alist) as a map.
   On putback, unmodified key-value pairs appear first and in their original
   order, then followed by new or updated key-value pairs in unspecified order.
   Example:
   # (update [[:a 1] [:b 2] [:c 3]]
       append-on-assoc assoc :a 5 :d 6)
   # [[:b 2] [:c 3] [:d 6] [:a 5]]"
  [pairs hash]
  :fetch (into {} pairs)
  :putback (loop [kvs [] okvs pairs m hash]
             (if-let [[[k v :as kv] & okvs] okvs]
               (if (= (get m k m) v)
                 (recur (conj kvs kv) okvs (dissoc m k))
                 (recur kvs okvs m))
               (into kvs m))))

(def ^:private transitions {})

(defn deftransition [from seg-or-seg-type to]
  (let [{to :type :as transition-info} (if (map? to) to {:type to})]
    (when-not (and (namespace from) (namespace to))
      (throw (ex-info "from and to must be namespaced" {:from from :to to})))
    (alter-var-root #'transitions update-in [from seg-or-seg-type] merge transition-info)))

(defn deftransitions [transitions-map]
  (doseq [[from to-map] transitions-map
          [seg-type to] to-map]
    (deftransition from seg-type to)))

(defn seg-types [seg]
  (let [e (-expr seg)
        type (cond
               (= e seg) (class seg)
               (symbol? e) e
               :else (first e))]
    (cons seg (cons type (ancestors type)))))

(defn transition-info [value-type seg]
  (when-let [to-map (get transitions value-type)]
    (some #(get to-map %) (seg-types seg))))

(defn fetch-type [value-type seg]
  (:type (transition-info value-type seg)))
