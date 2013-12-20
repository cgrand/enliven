(ns enliven.core.plans
  (:require [enliven.core.segments :as seg]
    [enliven.core.actions :as action]))

;; a plan is a hierarchichal representation of a set of rules

(def empty-plan {:range (sorted-map-by seg/cmp-range)
                 :number (sorted-map)
                 :misc {}})

(declare plan)

(defn- plan-in [wip-plan path action]
  (let [wip-plan (or wip-plan empty-plan)]
    (if-let [[seg & segs] (seq path)]
      (update-in wip-plan [(seg/seg-class seg) seg]
        plan-in segs action)
      (assoc wip-plan :action (action/update-subs action plan)))))

(defn plan [rules]
  (reduce (fn [plan [path action]] (plan-in plan path action))
    empty-plan rules))

(declare execute)

(defn- exec [node plan stack]
  (execute plan node stack))

(defn execute [plan node stack]
  (if-let [action (:action plan)]
    (action/perform action node stack execute)
    (reduce (fn [node plan-by-seg]
      (reduce-kv (fn [node seg plan]
                  (seg/update node seg exec plan stack))
        node plan-by-seg))
      node (vals plan))))
