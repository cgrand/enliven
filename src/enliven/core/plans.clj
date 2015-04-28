(ns enliven.core.plans
  (:require
    [enliven.core.lenses :as lens]
    [enliven.core.actions :as action]))

;; a plan is a hierarchical representation of a set of rules
;; a rooted plan (rplan) is a map with keys: :node-type :node and :plan

(def empty-plan {})

(defn map-vals [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

#_(defn map-actions [[node-type node plan] f]
   (if-let [action (:action plan)]
     (assoc plan :action (f node-type node action))
     (map-vals plan
       (fn [plans-by-seg]
         (reduce-kv (fn [plans-by-seg seg plan]
                      (assoc plans-by-seg seg
                        (map-actions [(lens/fetch node seg)
                                      (lens/fetch-type node-type seg)
                                      plan] f)))
           plans-by-seg plans-by-seg)))))

;; building a plan
(defn- nest [action]
  (letfn [(nest-rules [rules]
            (set (for [[path action] rules]
                   [path (nest action)])))]
    (-> action (assoc :scope-idx (inc (:scope-idx action)))
      (action/update :subs (lens/af nest-rules 1)))))

(defn- mash [action path sub-action]
  (cond
    (seq (lens/decompose path))
    (let [path (lens/decompose path)
          sub-action (if (:arg action) (nest sub-action) sub-action)]
      (action/update action :subs
        (fn [[skip-lens sub]]
          (if-let [path (lens/rm-prefix path skip-lens)]
            [skip-lens (conj sub [path sub-action])]
            [skip-lens sub]))))
    (= (action/update action :subs first) (action/update sub-action :subs first))
    (let [subs (into {} (:subs sub-action))]
      (action/update action :subs
        (fn [[skip-lens sub]]
          [skip-lens (into (set sub) (subs skip-lens))])))
    :else (throw (ex-info "Conflicting actions" {:actions [action sub-action]}))))

;; so, a plan is a map of segments to actions
;; there's a special action which is a pure group
;; when one rule is added to a plan there's only 4 possible scenarios:
;; * the rule dominates some existing rules: the rules are mashed into the new one
;; * the rule has a common (strict, non-empty) prefix with some existing rules: the new one and
;;   the existing ones are grouped together under the prefix. There can't be more than one strict
;;   non-empty prefix.
;;   sketch of proof:
#_[[common rem-lens rem-lens-a] (lens/gcl lens lens-a)
  [common' rem-lens' rem-lens-b] (lens/gcl lens lens-b)
  [very-common _ _] (lens/gcl common common')]
;;   if common and common' are different then very-common is non-empty which means that
;;   there should be a common prefix to lens-a/lens-b which is impossible by contruction
;; * the rule has an empty prefix with all existing rules: the rule is added to the plan
;; * no prefix (that is: not even an empty one) can be found with an existing rule: it's a conflict

(defn- sub-action
  "Creates an action for the given subplan"
  [subplan]
  {:op ::sub :scope-idx 0 :subs [subplan]})

(defn- conj-rule [plan [lens action]]
  (let [groups (group-by ffirst (for [[lens' :as lens+action] plan]
                                  [(lens/gcl lens lens') lens+action]))
        groups (dissoc groups lens/identity)]
    (if-let [conflicting-rules (groups nil)]
      (throw (ex-info "Conflicting lenses" {:lens lens :lenses (map (comp first second) conflicting-rules)}))
      (case (count groups)
        0 (assoc plan lens action)
        1 (let [[common group] (first groups)
               plan (reduce (fn [plan [_ [lens]]] (dissoc plan lens))
                      plan group)]
            (assoc plan common
              (if (some (fn [[[_ _ rem-lens]]] (lens/identity? rem-lens)) group)
                (if (= 1 (count group))
                  (let [[[_ rem-lens _] [_ action']] (first group)]
                    (mash action' rem-lens action))
                  (throw (ex-info "Shattered lenses, fill a bug!" {:lens lens :group group})))
                (let [[[_ rem-lens _]] (first group)]
                  (if (lens/identity? rem-lens)
                    (reduce (fn [action [[_ _ rem-lens] [_ action']]]
                              (mash action rem-lens action')) action group)
                    (let [subplan (into {rem-lens action}
                                    (for [[[_ _ rem-lens] [_ action]] group]
                                      [rem-lens action]))]
                      (sub-action subplan)))))))
        (throw (ex-info "Shattered lenses, fill a bug!" {:lens lens :lenses (keys plan)}))))))

(defn rplan [node-type node rules]
  (let [plan (reduce conj-rule empty-plan rules)
        plan (reduce-kv
               (fn [plan lens action]
                 (assoc plan lens
                   (action/update action :subs
                     (let [node-type (lens/fetch-type node-type lens)
                           node (lens/fetch node lens)]
                       (fn [[skip-lens rules]]
                         (rplan (lens/fetch-type node-type skip-lens) (lens/fetch node skip-lens) rules))))))
               plan plan)]
    {:node-type node-type :node node :plan plan}))

(defn- lens-execution-order
  "Assumes lenses are keys of a plan: simplified and with no common prefix, so
   checking only the first segment is enough and we are also guaranteed they won't overlap.
   The order is: non-ordered lenses first (eg associative) then ordered lenses (indices and
   slices) in decreasing order."
  [la lb]
  (let [[a] (lens/decompose la)
        [b] (lens/decompose lb)
        [fa ta :as ba] (lens/bounds a)
        [fb tb :as bb] (lens/bounds b)]
    (cond
      (and ba bb) (if (<= tb fa) -1 +1)
      ba +1
      bb -1
      :else 0)))

;; end of building

;; transforming plans

(defn perform-dispatch-fn [action scopes] (:op action))

(defmulti perform
  "Performs the required action and returns the updated node."
  perform-dispatch-fn)

(defn execute [rplan scopes]
  (let [{:keys [node-type node plan]} rplan]
    (reduce (fn [node [seg action]]
              (lens/putback node seg (perform action scopes)))
      node (sort-by key lens-execution-order plan))))

(defmethod perform ::sub [{[sub-rplan] :subs} scopes]
  (execute sub-rplan scopes))

(defmethod perform ::action/replace [{n :scope-idx path :arg} scopes]
  (-> scopes (nth n) (lens/fetch path)))

(defmethod perform ::action/if [{n :scope-idx path :arg [then else] :subs} scopes]
  (if (-> scopes (nth n) (lens/fetch path))
    (execute then scopes)
    (execute else scopes)))

(defmethod perform ::action/dup [{n :scope-idx path :arg [sub] :subs} scopes]
  (mapcat (fn [item] (execute sub (conj scopes item)))
    (-> scopes (nth n) (lens/fetch path))))

;; ALPHA

(defmacro ^:private if-const-let [[name scopes n lens] then else]
  `(let [lens# ~lens]
     (if (lens/const? lens#)
      (let [~name (lens/fetch nil lens#)]
        ~then)
      (let [scope# (nth ~scopes ~n)]
        (if (= ::dynamic scope#)
          ~else
          (let [~name (lens/fetch scope# lens#)]
            ~then))))))

(defn const-perform-dispatch-fn [action scopes] (:op action))

(defmulti const-perform
  "Performs the required action and returns [updated-node remaining-action-or-nil]."
  const-perform-dispatch-fn)

(defn- shift-seg [seg delta]
  (cond
    (lens/slice? seg)
    (let [[from to] (lens/bounds seg)]
      (lens/slice (+ from delta) (+ to delta)))
    (number? seg)
    (+ seg delta)
    :else
    seg))

(defn- shift-first-seg [seg delta]
  (let [[seg & segs] (lens/decompose seg)]
    (lens/lens (cons (shift-seg seg delta) segs))))

(defn- shift-plan [plan delta]
  (if (zero? delta)
    plan
    (reduce-kv (fn [plan [seg action]]
                 (assoc plan (shift-first-seg seg delta) action))
      plan plan)))

(defn const-execute
  "Returns an updated rplan."
  [rplan scopes]
  (lens/update rplan (lens/juxt :node :plan)
    (fn [[node plan]]
      (reduce (fn [[node plan] [seg action]]
                (let [[node' action'] (const-perform action scopes)
                      ; shifting can only occur when seg is *only* a slice
                      ; (it can't happen when seg ends by a slice because of how plan is built)
                      plan (if (lens/slice? seg)
                             (shift-plan plan (- (count node') (count node)))
                             plan)]
                  [(lens/putback node seg node')
                   (if action'
                     (assoc plan seg action')
                     plan)]))
        [node {}] (sort-by key lens-execution-order plan)))))

(defmethod const-perform ::sub [{[sub-rplan] :subs} scopes]
  (let [sub-rplan' (const-execute sub-rplan scopes)]
    [(:node sub-rplan')
     (when (seq (:plan sub-rplan'))
       {:op ::sub :subs XXX})]))

(defmethod perform ::action/replace [{n :scope-idx path :arg} scopes]
  (-> scopes (nth n) (lens/fetch path)))

(defmethod perform ::action/if [{n :scope-idx path :arg [then else] :subs} scopes]
  (if (-> scopes (nth n) (lens/fetch path))
    (execute then scopes)
    (execute else scopes)))

(defmethod const-perform ::action/dup [{n :scope-idx path :arg [sub] :subs} scopes]
  (mapcat (fn [item] (const-execute sub (conj scopes item)))
    (-> scopes (nth n) (lens/fetch path))))

#_(defn meta-execute [f x plan]
    (reduce f x (sort-by key lens-execution-order plan)))

;;; DEAD CODE BELOW ????
(defn ^:private unplan [plan]
  {:pre [(or (map? plan) (nil? plan))]}
  (letfn [(unplan' [plan]
            (if-let [planned-action (:action plan)]
              [[() planned-action]]
              (for [plans-by-seg (vals plan)
                    [seg sub-plan] plans-by-seg
                    [path action] (unplan sub-plan)]
                [(cons seg path) action])))]
    (for [[path action] (unplan' plan)]
      [path action])))

(defn tidy
  ([a-plan] (tidy a-plan lens/simplify))
  ([a-plan tidy-path]
    (letfn [(tidy-rules [rules]
              (map (fn [[path action]]
                     [(tidy-path path) action]) rules))]
      (-> a-plan unplan tidy-rules plan))))

(defn perform-dispatch-fn [action scopes node call-sub] (:op action))

(defmulti perform
  "Performs the required action and returns the updated node."
  perform-dispatch-fn)

(defn execute [node plan scopes call-sub]
  (if-let [action (:action plan)]
    (perform action scopes node call-sub)
    (reduce (fn [node [seg plan]]
              (lens/update node seg execute plan scopes call-sub))
      node (concat (:misc plan) (rseq (:number plan)) (rseq (:range plan))))))

(defn- shift-seg [seg delta]
  (cond
    (lens/slice? seg)
    (let [[from to] (lens/bounds seg)]
      (lens/slice (+ from delta) (+ to delta)))
    (number? seg)
    (+ seg delta)
    :else
    seg))

(defn- spliceable [x]
  (if (or (nil? x) (sequential? x))
    x
    (list x)))

(defn- has-plan? [plan]
  (and plan (not= plan empty-plan)))

(defmulti const-perform
  perform-dispatch-fn)

(defn const-execute [node plan scopes call-sub]
  (if-let [action (:action plan)]
    (const-perform action scopes node call-sub)
    (cond
      (seq (:range plan))
      (let [[node _ plans-by-seg]
            (reduce
              (fn [[node' d plans-by-seg] [seg plan]]
                (let [seg (shift-seg seg d)
                      sub-node (lens/fetch node' seg)
                      [sub-node' plan'] (const-execute sub-node plan scopes call-sub)
                      delta (- (count sub-node') (count sub-node))]
                  [(lens/putback node' seg sub-node')
                   (+ d delta)
                   (assoc plans-by-seg seg plan')]))
              [node 0 (sorted-map)] (rseq (:range plan)))]
       [node (assoc empty-plan :range plans-by-seg)])
      (seq (:misc plan))
      (let [[node plans-by-seg]
            (reduce
              (fn [[node' plans-by-seg] [seg plan]]
                (let [sub-node (lens/fetch node' seg)
                      [sub-node' plan'] (const-execute sub-node plan scopes call-sub)]
                  [(lens/putback node' seg sub-node')
                   (if (has-plan? plan')
                     (assoc plans-by-seg seg plan')
                     plans-by-seg)]))
              [node (sorted-map)] (:misc plan))]
       [node (assoc empty-plan :misc plans-by-seg)])
      ; quick'n'dirty
      (seq (:number plan))
      (let [[node plans-by-seg]
            (reduce
              (fn [[node' plans-by-seg] [seg plan]]
                (let [sub-node (lens/fetch node' seg)
                      [sub-node' plan'] (const-execute sub-node plan scopes call-sub)]
                  [(lens/putback node' seg sub-node')
                   (if (has-plan? plan')
                     (assoc plans-by-seg seg plan')
                     plans-by-seg)]))
              [node (sorted-map)] (:number plan))]
       [node (assoc empty-plan :number plans-by-seg)]))))

(defmacro ^:private if-const-let [[name scopes n lens] then else]
  `(let [lens# ~lens]
     (if (lens/const? lens#)
      (let [~name (lens/fetch nil lens#)]
        ~then)
      (let [scope# (nth ~scopes ~n)]
        (if (= ::dynamic scope#)
          ~else
          (let [~name (lens/fetch scope# lens#)]
            ~then))))))

(defmethod const-perform ::action/replace [{n :scope-idx path :arg :as action} scopes node call-sub]
  (if-const-let [v scopes n path]
    [v empty-plan]
    [node (assoc empty-plan :action action)]))

(defmethod perform ::action/replace [{n :scope-idx path :arg} scopes node call-sub]
  (-> scopes (nth n) (lens/fetch path)))

(defmethod perform ::action/if [{n :scope-idx path :arg [then else] :subs} scopes node call-sub]
  (if (-> scopes (nth n) (lens/fetch path))
    (call-sub then scopes)
    (call-sub else scopes)))

(defmethod perform ::action/dup [{n :scope-idx path :arg [sub] :subs} scopes node call-sub]
  (mapcat (fn [item]
            (spliceable (call-sub sub (conj scopes item))))
    (-> scopes (nth n) (lens/fetch path))))

(defmulti ^:private propagate-drop-scope :op)

(defn- drop-scope
  ([plan] (drop-scope plan 0))
  ([plan n]
    (map-actions plan (fn [{m :scope-idx :as action}]
                        (propagate-drop-scope
                          (if (> m n)
                            (assoc action :scope-idx (dec m))
                            action)
                          n)))))

(defmethod propagate-drop-scope :default [action n]
  (action/update action :subs lens/update 2 drop-scope action n))

(defmethod propagate-drop-scope ::action/dup [action n]
  (action/update action :subs lens/update 2 drop-scope action (inc n)))

(defmethod const-perform ::action/dup [{n :scope-idx path :arg [sub] :subs :as action} scopes node call-sub]
  (prn 'DUP)
  (if-const-let [coll scopes n path]
    (let [[nodes plans-by-seg]
          (reduce
            (fn [[nodes plan-by-ranges] item]
              (let [[node-type node sub] (call-sub sub (conj scopes item))
                    sub (drop-scope sub)
                    snodes (spliceable node)
                    nodes' (into nodes snodes)]
                [nodes'
                 (if (has-plan? sub)
                   (assoc plan-by-ranges
                     (lens/slice (count nodes) (count nodes'))
                     (if (= snodes node)
                       sub
                       (assoc-in empty-plan [:number 0] sub)))
                   plan-by-ranges)]))
            [[] (sorted-map)] coll)]
      [nodes (assoc empty-plan :range plans-by-seg)])
      [node (assoc empty-plan
              ; subs should embed their node so that const-execute could be propagated
              :action action)]))

(defmethod const-perform ::action/if [{n :scope-idx path :arg [then else] :subs :as action} scopes node call-sub]
  (if-const-let [test scopes n path]
    (const-execute node (if test then else) scopes call-sub)
    [node (assoc empty-plan
            ; subs should embed their node so that const-execute could be propagated
            :action action)]))
