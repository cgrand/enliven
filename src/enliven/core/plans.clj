(ns enliven.core.plans
  (:require [enliven.core.segments :as seg]
    [enliven.core.actions :as action]))

;; a plan is a hierarchichal representation of a set of rules

(def empty-plan (sorted-map-by seg/cmp))

(declare plan)

(defn- plan-in [plan path action]
  (if-let [[seg & segs] (seq path)]
    (assoc plan seg (plan-in (get plan seg empty-plan) segs action))
    (assoc plan ::action (action/update-subs action plan))))

(defn plan [rules]
  (reduce (fn [plan [path action]] (plan-in plan path action))
    empty-plan rules))

(declare execute)

(defn- exec [node plan stack]
  (execute plan node stack))

(defn execute [plan node stack]
  (if-let [action (::action plan)]
    (action/perform action node stack execute)
    (reduce-kv (fn [node seg plan]
                 (seg/update node seg exec plan stack))
      node plan)))
