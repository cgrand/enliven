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

(defmulti ^:private mash (fn [[op :as planned-action] path sub-action]
                           op))

(defmethod mash :default [planned-action path sub-action]
  (throw (ex-info "Conflicting rules" 
           {:planned-action planned-action
            :path path 
            :sub-action sub-action})))

(def ^:private nest
  (letfn [(nest [action]
            (-> action (assoc 1 (inc (nth action 1)))
              (action/update-subs nest-rules)))
          (nest-rules [rules]
            (set (for [[path action] rules] 
                   [path (nest action)])))]
    nest))

(defmethod mash ::action/dup [[op n args sub-plan] path sub-action]
  [op n args (plan-in sub-plan path (nest sub-action))])

(defmethod mash ::action/discard
  [planned-action _]
  planned-action)

(defn unplan [plan]
  (if-let [planned-action (:action plan)]
    (let [action (action/update-subs planned-action unplan)]
      [[() action]])
    (for [plans-by-seg (vals plan)
          [seg sub-plan] plans-by-seg 
          [path action] (unplan sub-plan)]
      [(conj path seg) action])))

(defn- plan-in [wip-plan path action]
  {:pre [(or (nil? path) (sequential? path))]}
  (let [wip-plan (or wip-plan empty-plan)]
    (if-let [[seg & segs] (seq path)]
      (if-let [planned-action (:action wip-plan)]
        (assoc empty-plan
          :action (mash planned-action path action))
        (update-in wip-plan [(seg/seg-class seg) seg]
          plan-in segs action))
      (let [planned-action (action/update-subs action plan)]
        (if-let [other-action (:action wip-plan)]
          (if (= other-action planned-action)
            wip-plan
            (throw (ex-info "Conflicting actions" {:actions [other-action planned-action]})))
          (plan
            (assoc empty-plan :action planned-action)
            (unplan wip-plan)))))))

(defmulti perform (fn [[op] stack node] op))

(defn execute [node plan stack]
  (prn node plan stack)
  (if-let [action (:action plan)]
    (perform action stack node)
    (reduce (fn [node [seg plan]]
              (seg/update node seg execute plan stack))
      node (concat (:misc plan) (rseq (:number plan)) (rseq (:range plan))))))

(defmethod perform ::action/replace [[op n [path]] stack node]
  (-> stack (nth n) (path/fetch-in path)))

(defmethod perform ::action/discard [[op n [path]] stack node]
  nil)

(defmethod perform ::action/if [[op n [path] then else] stack node]
  (if (-> stack (nth n) (path/fetch-in path))
    (execute node then stack)
    (execute node else stack)))

(defmethod perform ::action/dup [[op n [path] sub] stack node]
  (map (fn [item]
         (execute node sub (conj stack item)))
    (-> stack (nth n) (path/fetch-in path))))
