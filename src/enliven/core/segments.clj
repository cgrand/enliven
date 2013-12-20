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

(doseq [t [clojure.lang.Keyword clojure.lang.Symbol String]]
  (extend t
    Segment {:-fetch #(get %2 %1)
             :-putback #(assoc %2 %1 %3)
             :-expr identity}))

(extend Number
  Segment {:-fetch #(get %2 %1)
           :-putback #(assoc %2 %1 %3)
           :-expr identity})

(defn- bound [mn n mx]
  (-> n (max mn) (min mx)))

(extend-type clojure.lang.APersistentVector
  Segment 
  (-fetch [[from to] x]
    (let [n (count x)]
      (subvec x (bound 0 from n) (bound 0 to n))))
  (-putback [[from to] x v]
    (let [n (count x)]
      (-> x 
        (subvec 0 (bound 0 from n))
        (into (if (sequential? v) v (list v)))
        (into (subvec x (bound 0 to n) n)))))
  (-expr [seg] seg))

(defn seg-class [seg]
  (cond
    (vector? seg) :range
    (number? seg) :number
    :else :misc))

(defn bounds [seg v]
  (let [[from to] (case (seg-class seg)
                    :range seg
                    :number [seg (inc seg)])
        n (count v)]
    [(bound 0 from n) (bound 0 to n)]))

(defmacro defsegment [name [value-arg subvalue-arg & args] & methods]
  (let [fqname (symbol (clojure.core/name (ns-name *ns*)) 
                 (clojure.core/name name))
        auto-segment (empty? args)
        [args [_ & rest-arg]] (split-with #(not= (clojure.core/name %) "&") args)
        plain-args (take (count args) (repeatedly gensym))
        plain-rest-arg (when rest-arg (gensym))
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
                 Object
                 (equals [this# that#]
                   (and (satisfies? Segment that#)
                     (= (-expr this#) (-expr that#))))
                 (hashCode [this#] (hash (-expr this#)))
                 (toString [this#] (pr-str (-expr this#))))))]
    `(def ~name ~(if auto-segment (list f) f))))

(defn cmp-range
  "Compare disjoint ranges."
  [a b]
  (cond
    (= a b) 0
    (<= (nth a 1) (nth b 0)) -1
    :else +1))
