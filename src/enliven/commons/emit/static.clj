(ns enliven.commons.emit.static
  (:require [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.segments :as seg]
    [enliven.core.paths :as path]))

(defn tight-emit! 
  ([] [])
  ([acc]
    (let [p (peek acc)]
      (if (instance? StringBuilder p)
        (-> acc pop (conj (str p)))
        acc)))
  ([acc x]
    (let [p (peek acc)]
      (cond
        (fn? x) (conj (if (instance? StringBuilder p)
                        (-> acc pop (conj (str p)))
                        acc)
                  x)
        (instance? StringBuilder p) (do (.append ^StringBuilder p x) acc)
        :else (conj acc (StringBuilder. (str x)))))))

(defn tight-fn-emit! 
  ([] [(fn [emit' acc stack] acc) nil])
  ([[f ^StringBuilder sb]]
    (if sb
      (let [s (str sb)]
        (fn [emit' acc stack]
          (emit' (f emit' acc stack) s)))
      f))
  ([[f ^StringBuilder sb] x]
    (cond
      (fn? x) [(if sb
                (let [s (str sb)]
                  (fn [emit' acc stack]
                    (x emit' (emit' (f emit' acc stack) s) stack)))
                (fn [emit' acc stack]
                  (x emit' (f emit' acc stack) stack)))
               nil]
      sb [f (.append sb x)]
      :else [f (StringBuilder. (str x))])))

(defn render* 
  "Similar to render but takes a stack of scopes instead of just the model."
  [emitted stack f acc]
  (cond 
    (fn? emitted) (emitted f acc stack)
    (string? emitted) (f acc emitted)
    (char? emitted) (f acc (str emitted))
    :else (reduce #(render* %2 stack f %1) acc emitted)))

(defn render
  "Renders a compiled (emitted) template. Either as a string or using a reduce-like interface."
  ([emitted data]
    (str (render emitted data (fn [^StringBuilder sb s] (.append sb s))
          (StringBuilder.))))
  ([emitted data f acc]
    #_(emitted f acc (list data))
    (render* emitted (list data) f acc)))

(defn compose-encoding
  [emit enc]
  (fn encoded-emit
    ([] (emit))
    ([acc] (emit acc))
    ([acc x]
      (emit acc (if (fn? x)
                  (fn [emit' acc stack]
                    (x (compose-encoding emit' enc) acc stack))
                  (enc x))))))

(defmulti perform (fn [[op] stack render emit' acc] op))

(defmethod perform ::action/replace [[op n [f]] stack render emit' acc]
  (render (-> stack (nth n) f) nil emit' acc))

(defmethod perform ::action/discard [_ stack render emit' acc]
  acc)

(defmethod perform ::action/if [[op n [f] then else] stack render emit' acc]
  (if (-> stack (nth n) f)
    (render* then stack emit' acc)
    (render* else stack emit' acc)))

(defmethod perform ::action/dup [[op n [f] sub] stack render emit' acc]
  (reduce (fn [acc item]
            (render* sub (conj stack item) emit' acc))
    acc (-> stack (nth n) f)))

(defn prerender-action [node action prerender emit acc]
  (let [action (-> action 
                 (action/update-subs 
                   (fn [subplan] 
                     (emit (prerender node subplan emit (emit)))))
                 (action/update-paths path/fetcher-in))]
    (emit acc (fn [emit' acc stack]
                (perform action stack prerender emit' acc)))))

(defn prerender-unknown
  "When the plan involves unknown segments, fall back to the naive execution model."
  ([node plan prerender emit acc]
    (if-let [action (:action plan)]
      (prerender-action node action prerender emit acc)
      (emit acc (fn [emit' acc stack]
                  (-> plan (plan/execute node stack) 
                    (prerender nil emit' acc)))))))

(defn prerender-fragment
  [nodes plan prerender emit acc]
  (let [emit-consts (fn [acc nodes]
                      (reduce (fn [acc node]
                                (prerender node nil emit acc)) 
                        acc nodes))]
    (cond
      (nil? plan) (emit-consts acc nodes)
      (:action plan) (prerender-unknown nodes plan prerender emit acc)
      ; there should be a check that we only have known segments
      :else (let [[acc nodes-left]
                  (reduce 
                    (fn [[acc nodes-left] [x subplan]]
                      (let [[from to] (seg/bounds x nodes)
                            subnode (seg/fetch nodes-left x)]
                        [(as-> (emit-consts acc (subvec nodes-left 0 from)) acc
                           (prerender subnode subplan emit acc))
                         (subvec nodes-left to)])) 
                    [acc (vec nodes)] (concat (:number plan)
                                        (:range plan)))]
              (emit-consts acc nodes-left)))))
