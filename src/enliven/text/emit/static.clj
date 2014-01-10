(ns enliven.text.emit.static
  (:require [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.segments :as seg]
    [enliven.commons.emit.static :refer [tighten render*]]))

(declare emit-chars)

(defn emit-action [node plan]
  (if-let [action (:action plan)]
    (let [action (action/update-subs action #(-> node (emit-chars %) tighten))]
      (fn [f acc stack]
        (render* (action/perform action node stack
                   (fn [emitted node stack]
                     (fn [f acc _]
                       (render* emitted stack f acc))))
          nil f acc)))
    (fn [f acc stack]
      (-> plan (plan/execute node stack) (emit-chars nil) (render* nil f acc)))))

(defn emit-chars [chars plan]
  (cond
    (nil? plan) (apply str chars)
    (:action plan) (emit-action chars plan)
    :else (let [[emitted chars-left]
                (reduce 
                  (fn [[emitted chars-left] [x subplan]]
                    (let [[from to] (seg/bounds x chars)
                          subnode (seg/fetch chars-left x)]
                      [(conj emitted (apply str (subvec chars-left to))
                         (emit-chars subnode subplan))
                       (subvec chars-left 0 from)])) 
                  [() (vec chars)] (concat (:number plan)
                                     (:range plan)))]
            (conj emitted (emit-chars chars-left nil)))))
