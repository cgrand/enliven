(ns enliven.core.plans
  (:require [enliven.core.segments :as seg]
    [enliven.core.actions :as action]))

;; a plan is a hierarchichal representation of a set of rules

(def empty-plan {:range (sorted-map)
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

(defmulti perform (fn [[op] stack exec node] op))

(defn execute [node plan stack]
  (if-let [action (:action plan)]
    (perform action stack node)
    (reduce-kv (fn [node seg plan]
                 (seg/update node seg execute plan stack))
      node (concat (:misc plan) (rseq (:number plan)) (rseq (:range plan))))))

(defmethod perform ::action/replace [[op n [path]] stack node]
  (-> stack (nth n) (get-in path)))

(defmethod perform ::action/discard [[op n [path]] stack node]
  nil)

(defmethod perform ::action/if [[op n [path] then else] stack node]
  (if (-> stack (nth n) (get-in path))
    (execute node then stack)
    (execute node else stack)))

(defmethod perform ::action/dup [[op n [path] sub] stack node]
  (map (fn [item]
         (execute node sub (conj stack item)))
    (-> stack (nth n) (get-in path))))
