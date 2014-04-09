(ns enliven.core.plans
  (:require
    [enliven.core.lenses :as lens]
    [enliven.core.actions :as action]))

;; a plan is a hierarchical representation of a set of rules

(def empty-plan {:range (sorted-map)
                 :number (sorted-map)
                 :misc {}})

(defn map-vals [m f]
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
      (sort-by (comp count first))
      (reduce (fn [plan [lens action]]
                (plan-in plan (lens/decompose lens) action)) empty-plan))
    (fn [action] (action/update action :subs (lens/af plan 1) #_(fn [[skip-lens sub]] [skip-lens (plan sub)])))))

(defn- nest [action]
  (letfn [(nest-rules [rules]
            (set (for [[path action] rules]
                   [path (nest action)])))]
    (-> action (assoc :scope-idx (inc (:scope-idx action)))
      (action/update :subs (lens/af nest-rules 1)
        #_(fn [[skip-lens sub]] [skip-lens (nest-rules sub)])))))

(defn- mash [action path sub-action]
  (cond
    (seq (lens/decompose path))
    (let [path (lens/decompose path)
          sub-action (if (:arg action) (nest sub-action) sub-action)]
      (action/update action :subs
        (fn [[skip-lens sub]]
          (if-let [path (lens/relativize path skip-lens)]
            [skip-lens (conj sub [path sub-action])]
            [skip-lens sub]))))
    (= (action/update action :subs first) (action/update sub-action :subs first))
    (let [subs (into {} (:subs sub-action))]
      (action/update action :subs
        (fn [[skip-lens sub]]
          [skip-lens (into (set sub) (subs skip-lens))])))
    :else (throw (ex-info "Conflicting actions" {:actions [action sub-action]}))))

(defn unplan [plan]
  {:pre [(map? plan)]}
  (letfn [(unplan' [plan]
            (if-let [planned-action (:action plan)]
              (let [action (action/update planned-action :subs
                             (lens/af unplan 1)
                             #_(fn [[skip-lens plan]] [skip-lens (unplan plan)]))]
                [[() action]])
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
                     [(tidy-path path) (action/update action :subs (lens/af tidy-rules 1))]) rules))]
      (-> a-plan unplan tidy-rules plan))))

(defn- plan-in [wip-plan lenses action]
  {:pre [(or (nil? lenses) (sequential? lenses))
         (map? action)]}
  (let [wip-plan (or wip-plan empty-plan)]
    (if-let [other-action (:action wip-plan)]
      (assoc wip-plan
        :action (mash other-action lenses action))
      (if-let [[lens & lenses] (seq lenses)]
        (update-in wip-plan [(lens/lens-class lens) lens]
          plan-in lenses action)
        (assoc wip-plan :action action)))))

(defn perform-dispatch-fn [action scopes node] (:op action))

(defmulti perform
  "Performs the required action and returns the updated node."
  perform-dispatch-fn)

(defn execute [node plan scopes]
  (if-let [action (:action plan)]
    (perform action scopes node)
    (reduce (fn [node [seg plan]]
              (lens/update node seg execute plan scopes))
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

(defn const-execute [node plan scopes]
  (if-let [action (:action plan)]
    (const-perform action scopes node)
    (cond
      (seq (:range plan))
      (let [[node _ plans-by-seg]
            (reduce
              (fn [[node' d plans-by-seg] [seg plan]]
                (let [seg (shift-seg seg d)
                      sub-node (lens/fetch node' seg)
                      [sub-node' plan'] (const-execute sub-node plan scopes)
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
                      [sub-node' plan'] (const-execute sub-node plan scopes)]
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
                      [sub-node' plan'] (const-execute sub-node plan scopes)]
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

(defmethod const-perform ::action/replace [{n :scope-idx path :arg :as action} scopes node]
  (if-const-let [v scopes n path]
    [v empty-plan]
    [node (assoc empty-plan :action action)]))

(defmethod perform ::action/replace [{n :scope-idx path :arg} scopes node]
  (-> scopes (nth n) (lens/fetch path)))

(defmethod perform ::action/if [{n :scope-idx path :arg [then else] :subs} scopes node]
  (if (-> scopes (nth n) (lens/fetch path))
    (execute node then scopes)
    (execute node else scopes)))

(defmethod perform ::action/dup [{n :scope-idx path :arg [[skip-lens sub]] :subs} scopes node]
  (mapcat (fn [item]
            (spliceable (execute node sub (conj scopes item))))
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
  (action/update action :subs drop-scope action n))

(defmethod propagate-drop-scope ::action/dup [action n]
  (action/update action :subs drop-scope action (inc n)))

(defmethod const-perform ::action/dup [{n :scope-idx path :arg [[skip-lens sub]] :subs :as action} scopes node]
  (if-const-let [coll scopes n path]
    (let [node (lens/fetch node skip-lens)
          [nodes plans-by-seg]
          (reduce
            (fn [[nodes plan-by-ranges] item]
              (let [[node sub] (const-execute node sub (conj scopes item))
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

(defmethod const-perform ::action/if [{n :scope-idx path :arg [then else] :subs :as action} scopes node]
  (if-const-let [test scopes n path]
    (const-execute node (if test then else) scopes)
    [node (assoc empty-plan
            ; subs should embed their node so that const-execute could be propagated
            :action action)]))
