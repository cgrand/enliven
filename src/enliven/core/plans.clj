(ns enliven.core.plans
  (:require [enliven.core.segments :as seg]
    [enliven.core.paths :as path]
    [enliven.core.actions :as action]))

;; a plan is a hierarchical representation of a set of rules

(def empty-plan {:range (sorted-map)
                 :number (sorted-map)
                 :misc {}})

(defn- map-vals [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

(defn- map-actions [plan f]
  (if-let [action (:action plan)]
    (assoc plan :action (f action))
    (map-vals plan
      (fn [plans-by-seg]
        (map-vals plans-by-seg #(map-actions % f))))))

(declare plan-in)

(defn plan [rules]
  (map-actions
    (->> rules
      (map (fn [[path action]] [(path/canonical path) action]))
      (sort-by (comp count first))
      (reduce (fn [plan [path action]] (plan-in plan path action)) empty-plan))
    (fn [action] (action/update action :subs plan))))

(defmulti ^:private -mash (fn [action path sub-action]
                            (:op action)))

(defn- mash [action path sub-action]
  (if (and (empty? path) (= action sub-action)) ; TODO false negatives
    action
    (-mash action path sub-action)))

(defmethod -mash :default [action path sub-action]
  (throw (ex-info "Conflicting rules" 
           {:action action
            :path path 
            :sub-action sub-action})))

(defn- nest [action]
  (letfn [(nest-rules [rules]
            (set (for [[path action] rules] 
                   [path (nest action)])))]
    (-> action (assoc :scope-idx (inc (:scope-idx action)))
      (action/update :subs nest-rules))))

(defn unplan [plan]
  (letfn [(unplan' [plan]
            (if-let [planned-action (:action plan)]
              (let [action (action/update planned-action :subs unplan)]
                [[() action]])
              (for [plans-by-seg (vals plan)
                    [seg sub-plan] plans-by-seg
                    [path action] (unplan sub-plan)]
                [(cons seg path) action])))]
    (for [[path action] (unplan' plan)]
      [(path/canonical path) action])))

(defmethod -mash ::action/dup [action path sub-action]
  (cond
    (seq path) (action/update action :subs conj [path (nest sub-action)])
    (= (dissoc action :subs) (dissoc sub-action :subs))
    (action/update action :subs into (first (:subs sub-action)))
    :else (throw (ex-info "Conflicting actions" {:actions [action sub-action]}))))

(defn canonical [a-plan]
  (-> a-plan unplan plan))

(defn- plan-in [wip-plan path action]
  {:pre [(or (nil? path) (sequential? path))
         (map? action)]}
  (let [wip-plan (or wip-plan empty-plan)]
    (if-let [other-action (:action wip-plan)]
      (assoc wip-plan
        :action (mash other-action path action))
      (if-let [[seg & segs] (seq path)]
        (update-in wip-plan [(seg/seg-class seg) seg]
          plan-in segs action)
        (assoc wip-plan :action action)))))

(defn perform-dispatch-fn [action scopes node] (:op action))

(defmulti perform
  "Performs the required action and returns the updated node."
  perform-dispatch-fn)

(defn execute [node plan scopes]
  (if-let [action (:action plan)]
    (perform action scopes node)
    (reduce (fn [node [seg plan]]
              (seg/update node seg execute plan scopes))
      node (concat (:misc plan) (rseq (:number plan)) (rseq (:range plan))))))

(defn- shift-seg [seg delta]
  (cond
    (seg/slice? seg)
    (let [[from to] (seg/bounds seg)]
      (seg/slice (+ from delta) (+ to delta)))
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

(defn const-execute [node plan scopes]
  (if-let [action (:action plan)]
    (const-perform action scopes node)
    (cond
      (seq (:range plan))
      (let [[node _ plans-by-seg]
            (reduce
              (fn [[node' d plans-by-seg] [seg plan]]
                (let [seg (shift-seg seg d)
                      sub-node (seg/fetch node' seg)
                      [sub-node' plan'] (const-execute sub-node plan scopes)
                      delta (- (count sub-node') (count sub-node))]
                  [(seg/putback node' seg sub-node')
                   (+ d delta)
                   (assoc plans-by-seg seg plan')]))
              [node 0 (sorted-map)] (rseq (:range plan)))]
       [node (assoc empty-plan :range plans-by-seg)])
      (seq (:misc plan))
      (let [[node plans-by-seg]
            (reduce
              (fn [[node' plans-by-seg] [seg plan]]
                (let [sub-node (seg/fetch node' seg)
                      [sub-node' plan'] (const-execute sub-node plan scopes)]
                  [(seg/putback node' seg sub-node')
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
                (let [sub-node (seg/fetch node' seg)
                      [sub-node' plan'] (const-execute sub-node plan scopes)]
                  [(seg/putback node' seg sub-node')
                   (if (has-plan? plan')
                     (assoc plans-by-seg seg plan')
                     plans-by-seg)]))
              [node (sorted-map)] (:number plan))]
       [node (assoc empty-plan :number plans-by-seg)]))))

(defmacro ^:private if-const-let [[name scopes n path] then else]
  `(let [path# ~path]
     (if (path/const? path#)
      (let [~name (path/fetch-in nil path#)]
        ~then)
      (let [scope# (nth ~scopes ~n)]
        (if (= ::dynamic scope#)
          ~else
          (let [~name (path/fetch-in scope# path#)]
            ~then))))))

(defmethod const-perform ::action/replace [{n :scope-idx [path] :args :as action} scopes node]
  (if-const-let [v scopes n path]
    [v empty-plan]
    [node (assoc empty-plan :action action)]))

(defmethod perform ::action/replace [{n :scope-idx [path] :args} scopes node]
  (-> scopes (nth n) (path/fetch-in path)))

(defmethod perform ::action/if [{n :scope-idx [path] :args [then else] :subs} scopes node]
  (if (-> scopes (nth n) (path/fetch-in path))
    (execute node then scopes)
    (execute node else scopes)))

(defmethod perform ::action/dup [{n :scope-idx [path] :args [sub] :subs} scopes node]
  (mapcat (fn [item]
            (spliceable (execute node sub (conj scopes item))))
    (-> scopes (nth n) (path/fetch-in path))))

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
  (action/update action :subs drop-scope action n))

(defmethod propagate-drop-scope ::action/dup [action n]
  (action/update action :subs drop-scope action (inc n)))

(defmethod const-perform ::action/dup [{n :scope-idx [path] :args [sub] :subs :as action} scopes node]
  (if-const-let [coll scopes n path]
    (let [[nodes plans-by-seg]
          (reduce
            (fn [[nodes plan-by-ranges] item]
              (let [[node sub] (const-execute node sub (conj scopes item))
                    sub (drop-scope sub)
                    nodes' (into nodes (spliceable node))]
                [(into nodes node)
                 (if (has-plan? sub)
                   (assoc plan-by-ranges
                     (seg/slice (count nodes) (count nodes'))
                     sub)
                   plan-by-ranges)]))
            [[] (sorted-map)] coll)]
      [nodes (assoc empty-plan :range plans-by-seg)])
    [node (assoc empty-plan
            ; subs should embed their node so that const-execute could be propagated
            :action action)]))

(defmethod const-perform ::action/if [{n :scope-idx [path] :args [then else] :subs :as action} scopes node]
  (if-const-let [test scopes n path]
    (const-execute node (if test then else) scopes)
    [node (assoc empty-plan
            ; subs should embed their node so that const-execute could be propagated
            :action action)]))
