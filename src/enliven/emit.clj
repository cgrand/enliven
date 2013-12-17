(ns enliven.emit
  (:require [enliven.core.segments :as seg]
    [enliven.core.plans :as plan]))

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

(declare emit render)

(defn emit-dynamic 
  ([node plan]
    (emit-dynamic node plan emit))
  ([node plan emit]
    (fn [f acc stack] (-> plan (plan/execute node stack) (emit nil) (render nil f acc)))))

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
    (::plan/action plan) (emit-dynamic attrs plan emit-attrs)
    (some seg/special-keyword? (keys plan)) (emit-dynamic attrs plan emit-attrs)
    :else
    (let [untoucheds (reduce attrs dissoc (keys plan))
          toucheds (reduce attrs dissoc (keys untoucheds))]
      [(emit-attrs untoucheds nil) (emit-dynamic toucheds plan emit-attrs)])))

(defn emit-fragment [nodes plan]
  (cond
    (nil? plan) (map #(emit % nil) nodes)
    (::plan/action plan) (emit-dynamic nodes plan)
    :else (let [[emitted nodes-left]
                (reduce 
                  (fn [[emitted nodes-left] [x subplan]]
                    (let [[from to] (seg/bounds x nodes)
                          subnode (seg/fetch nodes-left x)]
                      [(conj emitted (map #(emit % nil) (subvec nodes-left to))
                         (emit subnode subplan))
                       (subvec nodes-left 0 from)])) 
                  [() (vec nodes)] (rseq plan))]
            (conj emitted (map #(emit % nil) nodes-left)))))

(defn emit-element [node plan]
  (cond
    (::plan/action plan) (emit-dynamic node plan)
    (known-segs-only? plan #{:tag :attrs :content}) ; includes fully static
      (let [tag (emit-tag (:tag node) (:tag plan))]
        ["<" tag 
         (emit-attrs (:attrs node) (:attrs plan))
         ">" (emit-fragment (:content node) (:content plan))
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

(defn render
  ([emitted data]
    (str (render emitted data (fn [^StringBuilder sb s] (.append sb s))
          (StringBuilder.))))
  ([emitted data f acc]
    (cond 
      (fn? emitted) (emitted f acc (list data))
      (string? emitted) (f acc emitted)
      :else (reduce #(render %2 data f %1) acc emitted))))
