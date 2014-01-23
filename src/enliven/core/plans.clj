(ns enliven.core.plans
  (:require [enliven.core.segments :as seg]
    [enliven.core.paths :as path]
    [enliven.core.actions :as action]))

;; a plan is a hierarchichal representation of a set of rules

(def empty-plan {:range (sorted-map)
                 :number (sorted-map)
                 :misc {}})

(declare plan-in)

(defn plan
  ([rules] 
    (plan empty-plan rules))
  ([wip-plan rules]
    (reduce (fn [wip-plan [path action]]
              (plan-in wip-plan path action))
      wip-plan rules)))

(defmulti ^:private -mash (fn [[op :as planned-action] path sub-action]
                            op))

(defn- mash [planned-action path sub-action]
  (if (and (empty? path) (= planned-action (action/update-subs sub-action plan)))
    planned-action
    (-mash planned-action path sub-action)))

(defmethod -mash :default [planned-action path sub-action]
  (throw (ex-info "Conflicting rules" 
           {:planned-action planned-action
            :path path 
            :sub-action sub-action})))

(defn- nest [action]
  (letfn [(nest-rules [rules]
            (set (for [[path action] rules] 
                   [path (nest action)])))]
    (-> action (assoc 1 (inc (nth action 1)))
      (action/update-subs nest-rules))))

(defmethod -mash ::action/discard
  [planned-action _]
  planned-action)

(defn unplan [plan]
  (letfn [(unplan' [plan]
            (if-let [planned-action (:action plan)]
              (let [action (action/update-subs planned-action unplan)]
                [[() action]])
              (for [plans-by-seg (vals plan)
                    [seg sub-plan] plans-by-seg
                    [path action] (unplan sub-plan)]
                [(cons seg path) action])))]
    (for [[path action] (unplan' plan)]
      [(path/canonical path) action])))

(defmethod -mash ::action/dup [[op n args sub-plan :as action] path sub-action]
  (cond
    (seq path) [op n args (plan-in sub-plan path (nest sub-action))]
    (= [op n args] (take 3 sub-action))
    [op n args (plan (into (set (unplan sub-plan)) (nth sub-action 3)))]
    :else (throw (ex-info "Conflicting actions" {:actions [action sub-action]}))))

(defn canonical [a-plan]
  (-> a-plan unplan plan))

(defn- plan-in [wip-plan path action]
  {:pre [(or (nil? path) (sequential? path))]}
  (let [wip-plan (or wip-plan empty-plan)]
    (if-let [planned-action (:action wip-plan)]
      (assoc empty-plan
        :action (mash planned-action path action))
      (if-let [[seg & segs] (seq path)]
       (update-in wip-plan [(seg/seg-class seg) seg]
         plan-in segs action)
       (plan
         (assoc empty-plan :action (action/update-subs action plan))
         (unplan wip-plan))))))

(defmulti perform
  "Performs the required action and returns the updated node."
  (fn [[op] scopes node] op))

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
  (fn [[op] scopes node] op))

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

(defmethod const-perform ::action/replace [[op n [path] :as action] scopes node]
  (if-const-let [v scopes n path]
    [v empty-plan]
    [node (assoc empty-plan :action action)]))

(defmethod perform ::action/replace [[op n [path]] scopes node]
  (-> scopes (nth n) (path/fetch-in path)))

(defmethod perform ::action/discard [[op n [path]] scopes node]
 nil)

(defmethod perform ::action/if [[op n [path] then else] scopes node]
  (if (-> scopes (nth n) (path/fetch-in path))
    (execute node then scopes)
    (execute node else scopes)))

(defmethod perform ::action/dup [[op n [path] sub] scopes node]
  (mapcat (fn [item]
            (spliceable (execute node sub (conj scopes item))))
    (-> scopes (nth n) (path/fetch-in path))))

(defmulti ^:private propagate-drop-scope (fn [[op] n] op))

(defn- map-vals [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

(defn- map-actions [plan f]
  (if-let [action (:action plan)]
    (assoc plan :action (f action))
    (map-vals plan
      (fn [plans-by-seg]
        (map-vals plans-by-seg #(map-actions % f))))))

(defn- drop-scope
  ([plan] (drop-scope plan 0))
  ([plan n]
    (map-actions plan (fn [[op m :as action]]
                        (propagate-drop-scope
                          (if (> m n)
                            (assoc action 1 (dec m))
                            action)
                          n)))))

(defmethod propagate-drop-scope :default [action n]
  (action/update-subs action drop-scope action n))

(defmethod propagate-drop-scope ::action/dup [action n]
  (action/update-subs action drop-scope action (inc n)))

(defmethod const-perform ::action/dup [[op n [path] sub :as action] scopes node]
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
