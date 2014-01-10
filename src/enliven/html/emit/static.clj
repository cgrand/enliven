(ns enliven.html.emit.static
  (:require [enliven.core.segments :as seg]
    [enliven.commons.emit.static :refer [tighten render*]]
    enliven.text enliven.text.emit.static
    [enliven.core.plans :as plan]
    [enliven.core.actions :as action]))

(defn known-segs-only? [plan known-segs]
  (every? known-segs (keys plan)))

(declare emit)

(defn emit-action
  "Compiles to fn when no clue of how to better compile."
  ([node plan]
    (emit-action node plan emit))
  ([node plan emit]
    (if-let [action (:action plan)]
      (let [action (action/update-subs action #(-> node (emit %) tighten))]
        (fn [f acc stack]
          (render* (action/perform action node stack
                     (fn [emitted node stack]
                       (fn [f acc _]
                         (render* emitted stack f acc))))
            nil f acc)))
      (fn [f acc stack]
        (-> plan (plan/execute node stack) (emit nil) (render* nil f acc))))))

(defn escape-text-node [^String text-node]
  (-> text-node (.replace "&" "&amp;") (.replace "<" "&lt;")))

(defn escape-attr-value [^String attr-value]
  (-> attr-value (.replace "&" "&amp;") (.replace "'" "&quot;")))

(defn emit-text-node [node plan]
  (if plan
    (if-let [char-plan (some-> plan :misc (get enliven.text/chars))]
      (enliven.text.emit.static/emit-chars
        (seg/fetch node enliven.text/chars)
        char-plan)
      (emit-action node plan))
    (escape-text-node node)))

(defn emit-tag [tag plan]
  (if plan
    (emit-action tag plan emit-tag)
    (name tag)))

(defn emit-attrs [attrs plan]
  (cond
    (nil? plan) (keep (fn [[attr v]] 
                        (cond 
                          (true? v) (name attr) 
                          v [" " (name attr) "='" (escape-attr-value v) "'"])) attrs)
    (:action plan) (emit-action attrs plan emit-attrs)
    (not-every? keyword? (keys (:misc plan))) (emit-action attrs plan emit-attrs)
    :else
    (let [untoucheds (reduce dissoc attrs (keys (:misc plan)))
          toucheds (reduce dissoc attrs (keys untoucheds))]
      [(emit-attrs untoucheds nil) (emit-action toucheds plan emit-attrs)])))

(defn emit-fragment [nodes plan]
  (cond
    (nil? plan) (map #(emit % nil) nodes)
    (:action plan) (emit-action nodes plan)
    :else (let [[emitted nodes-left]
                (reduce 
                  (fn [[emitted nodes-left] [x subplan]]
                    (let [[from to] (seg/bounds x nodes)
                          subnode (seg/fetch nodes-left x)]
                      [(conj emitted (map #(emit % nil) (subvec nodes-left to))
                         (emit subnode subplan))
                       (subvec nodes-left 0 from)])) 
                  [() (vec nodes)] (concat (:number plan)
                                     (:range plan)))]
            (conj emitted (map #(emit % nil) nodes-left)))))

(defn emit-element [node plan]
  (cond
    (:action plan) (emit-action node plan)
    (known-segs-only? (:misc plan) #{:tag :attrs :content}) ; includes the fully static case
      (let [plan-by-seg (:misc plan)
            tag (emit-tag (:tag node) (:tag plan-by-seg))]
        ["<" tag 
         (emit-attrs (:attrs node) (:attrs plan-by-seg))
         ">" (emit-fragment (:content node) (:content plan-by-seg))
         "</" tag ">"])
    :else ; dynamic for now but we could do a bit better 
    (emit-action node plan)))

(defn emit
  "Compiles to T where T is String | Fn | coll of T."
  [node plan]
  (cond
    (string? node) (emit-text-node node plan)
    (:tag node) (emit-element node plan)
    (sequential? node) (emit-fragment node plan)
    (nil? node) nil
    :else (throw (ex-info "Unexpected node" {:node node}))))
