(ns enliven.core
  (:refer-clojure :exclude [update-in get-in])
  (:require
    [clojure.core :as clj]
   #_[enliven.xover :as x]))

(alias '*ns* (ns-name *ns*))

;; Paths are vectors of PathSegments
(defprotocol PathSegment ; a lens
  (-get [seg x] 
    "Returns the sub-value of x corresponding to this segment.")
  (-update [seg node f]
    "Updates the sub-value of x corresponding to this segment. 
     Returns the updated x.
     Optional for read-only segments.")
  (may-match? [seg key]
    "Returns true, :maybe or falsy"))

(defn get-in [node path]
  (reduce #(-get %2 %1) node path))

(defn- updater 
  ([path f]
    (reduce (fn [f seg] #(-update seg % f)) f (rseq path)))
  ([path f & args]
    (updater path #(apply f % args))))

(defn update-in [node path f & args]
  ((updater path #(apply f % args)) node))

(defn bounds [x]
  ; TODO: sets etc.
  (cond
    (number? x) [x (inc x)]
    (vector? x) x))

(defn- compare-segments [a b]
  (if-let [[froma toa] (bounds a)]
    (if-let [[fromb tob] (bounds b)]
      (cond
        (<= toa fromb) 1
        (<= tob froma) -1
        :else 0)
      1)
    (if (bounds b) -1 
      (compare a b))))

(defn- compare-paths [a b]
  (if-let [[a & as] (seq a)]
    (if-let [[b & bs] (seq b)]
      (let [cmp (compare-segments a b)]
        (if (zero? cmp)
          (recur as bs)
          cmp))
      0)
    0))

(defn- ordered-by-path? [path-key coll]
  (not-any? (fn [[a b]] (pos? (compare-paths a b))) (partition 2 1 (map path-key coll))))

(defn- nest-by-path [path-key coll]
  (reduce #(let [path (path-key %2)]
             (clj/update-in %1 (conj path ::value) 
               (fnil conj #{}) %2)) {} coll))

(defn- unnest-by-path [m]
  (concat (::value m) 
    (mapcat unnest-by-path (vals (dissoc m ::value)))))

(defn- prefix? [prefix path]
  (= prefix (subvec path 0 (count prefix))))

(defn- remove-prefix [prefix path]
  (subvec path (count prefix)))

(defn- sort-vals [left-to-right m]
  (if-let [items (::value m)]
    (cond 
      (next items)
        (throw (ex-info "Conflicting transforms"
                 {:transforms items}))
      (next m)
        (let [[path f args] (first items)
              transforms (unnest-by-path (dissoc m ::value))
              additional-ts
              (for [[tag pp t :as arg] args]
                (case tag
                  :sub (filter #(prefix? pp (first %)) transforms)
                  nil))
              remaining-transforms
              (reduce disj (set transforms)
                (apply concat additional-ts))]
          (if (seq remaining-transforms)
            (throw (ex-info "Nested conflicting transforms"
                     {:transforms remaining-transforms}))
            [[path f 
              (map (fn [arg rules]
                     (if rules
                       (let [[tag pp t] arg]
                         [tag pp (into t
                                   (for [[p f args] rules]
                                     ; ungrounding...
                                     (into [(path-selector (remove-prefix pp p)) f] args)))])
                       arg)) 
                args additional-ts)]]))
      :else (seq items))
    (mapcat (fn [x] (sort-vals left-to-right (val x)))
      (sort-by key (if left-to-right
                     (comp - compare-segments)
                     compare-segments) m))))

(defn order-by-path
  ([path-key coll]
    (order-by-path false path-key coll))
  ([left-to-right path-key coll]
    (sort-vals left-to-right
      (nest-by-path path-key coll))))

;; all those types will be treated as associative segments
(def associative-pathsegment-types #{clojure.lang.Keyword String})

;; associative segments are read-write
;; sets are viewed as assocations of key to true
;; so if the updating fn returns a falsy value, the key is removed from the set.
(defn associative-get [k x] 
  (if (set? x)
    (contains? x k)
    (get x k)))

(defn associative-update [k m f] 
  (if (set? m)
    (if (f (contains? m k))
      (conj m k)
      (disj m k))
    (assoc m k (f (get m k)))))

(doseq [type associative-pathsegment-types]
  (extend type PathSegment
    {:-get associative-get
     :-update associative-update
     :may-match? =}))

(defn splice [items]
  (vary-meta items assoc ::splice true))

(defn splice? [coll]
  (-> coll meta ::splice))

(extend Number
  PathSegment
  {:-get associative-get
   :-update (fn [i coll f]
              (if (vector? coll)
                (let [x (f (nth coll i))]
                  (if (splice? x)
                    (-> (subvec coll 0 i) (into x) (into (subvec coll (inc i))))
                    (assoc coll i x)))
                (associative-update i coll f)))
   :may-match? =})

#_((defrecord QuotedKey [k]
   PathSegment
   (-get [_ m] (associative-get m k))
   (-update [_ m f] (associative-update m k f)))

(defn quote-key [k]
  (if (some #(instance? % k) associative-pathsegment-types) k (QuotedKey. k)))

(defn unquote-key [k]
  (if (instance? QuotedKey k) (:k k) k)))

(defmacro ^:private doubtful-or [x y]
  `(case ~x
     (nil false) ~y
     :maybe (or ~y :maybe)
     true))

(defmacro ^:private doubtful-and [x y]
  `(case ~x
     (nil false) false
     :maybe (and ~y :maybe)
     ~y))

(defn- subv
  ([v from] 
    (subvec v (min (max 0 from) (count v))))
  ([v from to]
    (subvec v (min (max 0 from) (count v)) (min (max 0 to) (count v)))))

(extend-protocol PathSegment
  clojure.lang.APersistentVector ; ranges
  (-get [[from to] x]
    (if (vector? x)
      (subv x from to)
      (->> x (take to) (drop from) vec)))
  (-update [[from to :as range] v f]
    (let [v (if (vector? v) v (vec v))]
      (-> (subv v 0 from)
        (into (f (-get range v)))
        (into (subv v to)))))
  (may-match? [[from to :as range] n]
    (and (number? n) (<= from n) (< n to)))
  clojure.lang.APersistentMap
  (-get [m x]
    (persistent! (reduce-kv (fn [r k seg] (assoc! r k (-get seg x))) (transient {}) m)))
  (-update [m x f] 
    (let [r (f (-get m x))
          r (if (ifn? r) r #(get r %))]
      (reduce-kv (fn [x k seg] (-update seg x (constantly (r k)))) x m)))
  (may-match? [m simple-seg]
    (reduce #(doubtful-or %1 (may-match? %2 simple-seg))
      false (vals m)))
  clojure.lang.APersistentSet
  (-get [s x]
    (persistent! (reduce (fn [r seg] (assoc! r seg (-get seg x))) (transient {}) s)))
  (-update [s x f] 
    (let [r (f (-get s x))
          r (if (ifn? r) r #(get r %))]
      (reduce (fn [x seg] (-update seg x (constantly (r seg)))) x s)))
  (may-match? [segs simple-seg]
    (reduce #(doubtful-or %1 (may-match? %2 simple-seg))
      false segs))
  Object ; default is to try to call it, read only
  (-get [seg x]
    (seg x))
  (may-match? [seg simple-seg] :maybe))

(defn path-segment [& {:keys [get put update]}]
  (if put
    (reify PathSegment
      (-get [_ x] (get x))
      (-update [_ x f] (put (f (get x)))))
    (reify PathSegment
      (-get [_ x] (get x))
      (-update [_ x f] (update x f)))))

;; a loc is a read-only cursor, you can move it only with up and down
;; updates are performed  using paths
(defrecord ^:private Loc [path nodes])

(defn loc
  "Creates a loc from a root node and moves it along an optional path."
  ([node]
    (loc node []))
  ([node path]
    (->Loc (vec path) (vec (reductions #(-get %2 %1) node path)))))

(defn down [loc seg]
  (let [nodes (:nodes loc)
        node (-get seg (peek nodes))]
    (->Loc (conj (:path loc) seg) (conj nodes node))))

(defn up [loc]
  (->Loc (pop (:path loc)) (pop (:nodes loc))))

(defn path [loc]
  (:path loc))

(defn node [loc]
  (peek (:nodes loc)))

;; Selectors 

(defprotocol Selector
  (-select [sel loc] "Starts from a loc and returns a coll of locs."))

(extend-protocol Selector
  clojure.lang.Fn
  (-select [f loc] (f loc))
  #_#_Object ; by default assumes it's a path segment
  (-select [seg loc]
   (list (down loc seg))))

(defn select [loc sel]
  (-select sel loc))

(defn locs [root sel]
  (select (loc root) sel))

(defn paths [root sel]
  (map path (locs root sel)))

(defn nodes [root sel]
  (map node (locs root sel)))

(def here "identity selector" list)

(defn chain
  "Composes several locs, from left to right."
  ([] here)
  ([sel] sel)
  ([sela selb]
    (cond
      (= here sela) selb
      (= here selb) sela
      :else (reify Selector
             (-select [_ loc]
               (for [loc (select loc sela)
                     loc (select loc selb)]
                 loc)))))
  ([sela selb & sels]
    (reduce chain (chain sela selb) sels)))

(defn loc-pred 
  "Returns a filtering selector which keeps only locs for which pred is true."
  [pred]
  (fn [loc]
    (when (pred loc) (here loc))))

(defn node-pred [pred]
  (loc-pred (comp pred node)))

(defn path-selector [path]
  (reify Selector
    (-select [_ loc]
      (here (reduce #(down %1 %2) loc path)))))

(defn const [v]
  {::const v})

(defn const? [x]
  (and (map? x) (contains? x ::const)))

(defn data-arg [x]
  (cond
    (vector? x) [:path x]
    (keyword? x) [:path [x]]
    (const? x) [:path [::const x]]
    :else (throw (IllegalArgumentException. (pr-str x)))))

;; Transformations

;; a transformation is a coll of [sel f & args] where:
;; an arg is either [:path data-path] or [:sub node-path t] where t
;; is a transformation or [:const x]
;; TODO: does it preclude recursive transformations?

;; Dec 4th 2013: attempting a tidying up
;; a transformation is a coll of [sel f args] where:
;; an arg is either [:path data-path] or [:sub sel t] where t
;; is a transformation 
;; (no more [:const x] (:const should be considered a special case of :path))

;; TODO: 
;; * switch from [sel f & args] to [sel f args]
;; * switch from [:sub path t] to [:sub sel t]
;; [DONE] remove :const, [:const x] becomes [:path [::const x]]
;; * create introspection code to determine what are the relevant paths
;; * create refactoring code to rename paths

;; a grounded transformation is a coll of [path f args] where:
;; an arg is either [:path data-path] or [:sub paths t] where t
;; is a grounded transformation 

(defn- plan? [plan]
  (every? #(and (vector? %) (= 3 (count %))) plan))

(defn ground-plans 
  "Returns a map from id to plans. Plans are coll of [path f args].
   The plan under the il key is the main plan."
  [t node]
  (let [ground-actions (order-by-path first
                         (for [[node-sel f & args] t
                               p (paths node node-sel)]
                           [p f (for [[tag pp t :as arg] args]
                                  (case tag 
                                    :sub [tag (into p pp) t]
                                    arg))]))
        ground-subs-ids (set (for [[_ _ args] ground-actions
                                 [tag :as arg] args
                                 :when (= :sub tag)]
                              arg))
        plans (reduce into {}
                (for [[tag p t :as id] ground-subs-ids
                      :let [node (get-in node p)
                            plan (ground-plans t node)]]
                  (-> plan (dissoc nil) (assoc id (plan nil)))))]
    (assoc plans nil ground-actions)))

(defn- order-by-dependency [ground-plans]
  (letfn [(order [id]
            (let [deps (set (for [[p f args] (get ground-plans id)
                                  [tag :as arg] args
                                  :when (= :sub tag)]
                              arg))]
              (distinct (concat (mapcat order deps) (list id)))))]
    (order nil)))

(defn- bind-rule [bound-plans [p f args]]
  (let [fargs (for [[tag p t :as arg] args]
                (case tag 
                  :sub (constantly (bound-plans arg))
                  :path (if (= ::const (first p))
                          (constantly (second p))
                          #(get-in % p))))
        fargs (if (seq args)
                (apply juxt fargs)
                (constantly nil)) ]
    #(apply update-in %1 p f (fargs %2))))

#_(defn- js-bind-rule [bound-plans [p f args]]
   (let [fargs (for [[tag p t :as arg] args]
                 (case tag 
                   :sub (constantly (bound-plans arg))
                   :path #(get-in % p)
                   :const (constantly p)))
         fargs (if (seq args)
                 (apply juxt fargs)
                 (constantly nil))]
      (fn [node-sym data-sym] 
        (list* f (emit-js-node-path node-sym p) (emit-js-args args data-sym)))))

(defn- bind-plan [bound-plans plan node]
  {:pre [(ordered-by-path? first plan)]}
  (let [plan (map #(bind-rule bound-plans %) plan)]
    (fn [data]
      (reduce (fn [node f] (f node data)) node plan))))

(defn- reduce-by [key f init coll]
  (persistent! 
    (reduce (fn [m x] (let [k (key x)]
                        (assoc! m (f (get m k init) x))))
      (transient {}) coll)))

(defmacro ^:private => [a b]
  `(or (not ~a) ~b))

(defn narrow-plans 
  "Narrows the plan to rules for each of the given segment. 
   Returns a map whose keys are the sepcified segments and
   ::unexpected.
   Values are plans with paths shortened."
  [plan segs]
  (let [segs (set segs)]
    (reduce-by (fn [[[seg]]]
                 (get segs seg ::unexpected)) 
      (fn [subplan [[seg & segs] :as rule]]
        (conj subplan (assoc rule 0 segs)))
      [] plan)))

#_(defn pre-render [node context segs-for plan render]
   {:pre [(=> (some (fn [[path]] (empty? path)) plan)
            (= (ffirst plan) []))]}
   (let [segs (segs-for context node)
         untouched-segs 
         (reduce
           (fn [segs [path]]
             (set (remove #(may-match? % path) segs)))
           segs plan)
         (reduce (fn [segs [path :as rule]]
                   (reduce-kv 
                     (fn [segs seg _]
                       (if (may-match? seg path)
                        (assoc segs seg
                          (conj (segs seg []) rule))
                        segs))
                     segs segs))
           segs plan)]
     (if (seq touched-segs-to-plans)
       (dynamic-node node context 
         (reduce-kv 
           (fn [m seg subplan]
             (assoc m seg 
               (pre-render (-get node seg) (segs seg) segs-for subplan)))
           {} touched-segs-to-plans))
       (static-node node))))

#_(defn html-segs-for [context node]
  (case context 
    :node (when (map? node)
            {:content :nodes, :attrs :attrs})
    :nodes 
    (zipmap (range (count node)) (constantly :node))
    :attrs (zipmap (keys node) (constantly :attr))
    nil))

#_(defn- js-bind-plan [bound-plans plan node]
   (let [plan (map #(bind-rule bound-plans %) plan)]
     (fn [data]
       (reduce (fn [node f] (f node data)) node plan))))

(defn bind 
  "Binds a transformation t to a specific node, allowing for optimization."
  [t root]
  (let [plans (ground-plans t root)
        ids (order-by-dependency plans)
        bound-plans (reduce 
                      (fn [bound-plans id]
                        (let [node (if-let [[tag p] id] (get-in root p) root)]
                          (assoc bound-plans id 
                            (bind-plan bound-plans (plans id) node))))
                      {} ids)]
    (get bound-plans nil)))

(defn transform 
  "Applies a transformation t to a node and some data, returns the transformed node."
  [t node data]
  ((bind t node) data))

(defn compose [& ts]
  (reduce into #{} ts))

(defn at [node-sel & ts]
  (let [t (compose ts)]
    (for [[sub-node-sel f & args] t]
      (into [(chain node-sel sub-node-sel) f] args))))

(defn scope-data [path & ts]
  (let [t (compose ts)]
    (for [[node-sel f & args] t]
      (into [node-sel f] (map (fn [[tag p :as arg]]
                                (if (= tag :path)
                                  [:path (into path p)]
                                  arg)) args)))))

(defmacro let-do [node-name? bindings & body]
  (let [[node-name bindings body] 
        (if (vector? node-name?) 
          [(gensym '_) node-name? (cons bindings body)]
          [node-name? bindings body])]
    `[[here (fn [~node-name ~@(take-nth 2 bindings)]
               ~@body)
      ~@(map #(vector :path %) (take-nth 2 (next bindings)))]]))
