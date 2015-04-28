(ns enliven.commons.emit.static
  (:require [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.lenses :as lens]))

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

#_(defn tight-eval-fn-emit!
   ([] [(map gensym '[emit' acc stack]) [] {} nil])
   ([[[emit'-sym acc-sym stack-sym] forms meta-args ^StringBuilder sb]]
     (let [forms (if sb
                   (conj forms (list emit'-sym acc-sym (str sb)))
                   forms)
           meta-f (eval `(fn [~@(keys meta-args)]
                           (fn [~emit'-sym ~acc-sym ~stack-sym]
                             (as-> ~acc-sym ~acc-sym ~@forms))))]
       (apply meta-f (vals meta-args))))
   ([[[emit'-sym acc-sym stack-sym :as syms] forms meta-args ^StringBuilder sb] x]
     (cond
       (fn? x) (let [f-sym (gensym 'f)]
                 [syms
                  (as-> forms forms
                    (if sb
                      (conj forms (list emit'-sym acc-sym (str sb)))
                      forms)
                    (conj forms (list f-sym emit'-sym acc-sym stack-sym)))
                  (assoc meta-args f-sym x)
                  nil])
       sb [syms forms meta-args (.append sb x)]
       :else [syms forms meta-args (StringBuilder. (str x))])))

(defn bytes-fn-emit!
  ([] [(fn [emit' acc stack] acc) nil])
  ([[f ^java.io.ByteArrayOutputStream out]]
    (if out
      (let [b (.toByteArray out)]
        (fn [emit' acc stack]
          (emit' (f emit' acc stack) b)))
      f))
  ([[f ^java.io.ByteArrayOutputStream out] x]
    (cond
      (fn? x) [(if out
                (let [b (.toByteArray out)]
                  (fn [emit' acc stack]
                    (x emit' (emit' (f emit' acc stack) b) stack)))
                (fn [emit' acc stack]
                  (x emit' (f emit' acc stack) stack)))
               nil]
      out [f (let [b (.getBytes ^String x "UTF-8")] (doto out (.write b 0 (alength b))))]
      :else [f (let [b (.getBytes ^String x "UTF-8")
                     n (alength b)]
                 (doto (java.io.ByteArrayOutputStream. n) (.write b 0 n)))])))

(defn render*
  "Similar to render but takes a stack of scopes instead of just the model."
  [emitted stack f acc]
  (cond
    (fn? emitted) (emitted f acc stack)
    (vector? emitted) (reduce #(render* %2 stack f %1) acc emitted)
    :else (f acc emitted)))

(defn render
  "Renders a compiled (emitted) template. Either as a string or using a reduce-like interface."
  ([emitted data]
    (str (render emitted data (fn [^StringBuilder sb s] (.append sb s))
          (StringBuilder.))))
  ([emitted data f acc]
    (render* emitted (list data) f acc)))

(defmulti perform (fn [action stack render emit' acc] (:op action)))

(defmethod perform ::action/replace [{n :scope-idx f :arg} stack render emit' acc]
  (render (-> stack (nth n) f) emit' acc))

#_(defmethod perform ::action/if [{n :scope-idx f :arg [then else] :subs} stack render emit' acc]
   (if (-> stack (nth n) f)
     (render* then stack emit' acc)
     (render* else stack emit' acc)))

(defmethod perform ::action/dup [{n :scope-idx f :arg [sub] :subs} stack render emit' acc]
  (reduce (fn [acc item]
            (render* sub (conj stack item) emit' acc))
    acc (-> stack (nth n) f)))

(defmulti prerenderer-fn identity)

(defn prerender [node-type node plan enc emit acc]
  ((prerenderer-fn node-type) node plan enc emit acc))

(defn prerender-action [node action node-type enc emit acc]
  (let [prerenderer (prerenderer-fn node-type)
        action (-> action
                 (action/update :subs
                   (fn [[skip-lens subplan]]
                     (emit (prerenderer (lens/fetch node skip-lens) subplan enc emit (emit)))))
                 (action/update :arg lens/fetcher))]
    (emit acc (fn [emit' acc stack]
                (perform action stack
                  (fn [node emit' acc]
                    (prerenderer node nil enc emit' acc)) emit' acc)))))

(defn prerender-unknown
  "When the plan involves unknown segments, fall back to the naive execution model."
  [node plan node-type enc emit acc]
  (if-let [action (:op plan)]
    (prerender-action node action node-type enc emit acc)
    (emit acc (let [prerenderer (prerenderer-fn node-type)]
                (fn [emit' acc stack]
                  (prerenderer (plan/execute node plan stack) nil enc emit' acc))))))

(defn prerender-nodes
  [nodes plan node-type enc emit acc]
  (let [prerenderer (prerenderer-fn node-type)
        emit-consts (fn [acc nodes]
                      (reduce (fn [acc node]
                                (prerenderer node nil enc emit acc))
                        acc nodes))]
    (cond
      (nil? plan) (emit-consts acc nodes)
      (:action plan) (prerender-unknown nodes plan node-type enc emit acc)
      ; there should be a check that we only have known segments
      :else (let [[acc prev-to]
                  (reduce
                    (fn [[acc prev-to] [x subplan]]
                      (let [[from to] (lens/bounds x nodes)
                            subnode (lens/fetch nodes x)]
                        [(as-> (emit-consts acc (subvec nodes prev-to from)) acc
                           (prerenderer subnode subplan enc emit acc))
                         to]))
                    [acc 0] (concat (:number plan) (:range plan)))]
              (emit-consts acc (subvec nodes prev-to))))))
