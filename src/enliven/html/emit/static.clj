(ns enliven.html.emit.static
  (:require [enliven.core.segments :as seg]
    [enliven.core.plans :as plan]
    [enliven.core.actions :as action]))

(defn- flat-reduce [f acc x]
  (if (or (fn? x) (string? x))
    (f acc x)
    (reduce (partial flat-reduce f) acc x)))

(defn tighten [x]
  (flat-reduce (fn [v x]
                 (if (and (string? (peek v)) (string? x))
                   (conj (pop v) (str (peek v) x))
                   (conj v x)))
    [] x))

(defn known-segs-only? [plan known-segs]
  (every? known-segs (keys plan)))

(declare emit render*)

(defn emit-dynamic 
  ([node plan]
    (emit-dynamic node plan emit))
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

(def escape-text-node str) ; TODO

(defn emit-text-node [node plan]
  (if plan
    (emit-dynamic node plan)
    (escape-text-node node)))

(defn emit-tag [tag plan]
  (if plan
    (emit-dynamic tag plan emit-tag)
    (name tag)))

(def escape-attr-value str) ; TODO

(defn emit-attrs [attrs plan]
  (cond
    (nil? plan) (keep (fn [[attr v]] 
                        (cond 
                          (true? v) (name attr) 
                          v [" " (name attr) "='" (escape-attr-value v) "'"])) attrs)
    (:action plan) (emit-dynamic attrs plan emit-attrs)
    (not-every? keyword? (keys (:misc plan))) (emit-dynamic attrs plan emit-attrs)
    :else
    (let [untoucheds (reduce dissoc attrs (keys (:misc plan)))
          toucheds (reduce dissoc attrs (keys untoucheds))]
      [(emit-attrs untoucheds nil) (emit-dynamic toucheds plan emit-attrs)])))

(defn emit-fragment [nodes plan]
  (cond
    (nil? plan) (map #(emit % nil) nodes)
    (:action plan) (emit-dynamic nodes plan)
    :else (let [[emitted nodes-left]
                (reduce 
                  (fn [[emitted nodes-left] [x subplan]]
                    (let [[from to] (seg/bounds x nodes)
                          subnode (seg/fetch nodes-left x)]
                      [(conj emitted (map #(emit % nil) (subvec nodes-left to))
                         (emit subnode subplan))
                       (subvec nodes-left 0 from)])) 
                  [() (vec nodes)] (concat (rseq (:number plan))
                                     (rseq (:range plan))))]
            (conj emitted (map #(emit % nil) nodes-left)))))

(defn emit-element [node plan]
  (cond
    (:action plan) (emit-dynamic node plan)
    (known-segs-only? (:misc plan) #{:tag :attrs :content}) ; includes the fully static case
      (let [plan-by-seg (:misc plan)
            tag (emit-tag (:tag node) (:tag plan-by-seg))]
        ["<" tag 
         (emit-attrs (:attrs node) (:attrs plan-by-seg))
         ">" (emit-fragment (:content node) (:content plan-by-seg))
         "</" tag ">"])
    :else ; dynamic for now but we could do a bit better 
    (emit-dynamic node plan)))

(defn emit [node plan]
  (cond
    (string? node) (emit-text-node node plan)
    (:tag node) (emit-element node plan)
    (sequential? node) (emit-fragment node plan)
    (nil? node) nil
    :else (throw (ex-info "Unexpected node" {:node node}))))

(defn render* [emitted stack f acc]
  (cond 
    (fn? emitted) (emitted f acc stack)
    (string? emitted) (f acc emitted)
    :else (reduce #(render* %2 stack f %1) acc emitted)))

(defn render
  ([emitted data]
    (str (render emitted data (fn [^StringBuilder sb s] (.append sb s))
          (StringBuilder.))))
  ([emitted data f acc]
    (render* emitted (list data) f acc)))
