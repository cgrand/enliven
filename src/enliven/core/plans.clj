(ns enliven.core.plans
  (:require
    [enliven.core.lenses :as lens]
    #_[enliven.core.actions :as action]
    [clojure.core.match :as m :refer [match]]))

;; there are three type of plans:
;; [:set data-lens]
;; [:dup data-lens subplan]
;; [:comp subplans] where subplans is a map of coprime src-lenses to plans.

(defn merge-plan
  ([] nil)
  ([a] a)
  ([a b]
  (let [id []] ; weird workaround 
    (match [a b]
      [nil b] b
      [a nil] a
      
      ;; remove unneeded nesting
      [{:plan-type :comp :subplans {id pa}} b] (merge-plan pa b)
      [a {:plan-type :comp :subplans {id pb}}] (merge-plan a pb)
      
      ;; dups merge only when same data-lens
      [{:plan-type :dup :data-lens dla :subplan pa} {:plan-type :dup :data-lens dlb :subplan pb}]
      (if (= dla dlb)
        {:plan-type :dup :data-lens dla :subplan (merge-plan pa pb)}
        (throw (ex-info "Conflicting dups" {:plans [a b]})))
      
      ;; one dup merges with anything else
      ;; (core.match's dragon: by default a symbol implies an equality check)
      [{:plan-type :dup :data-lens dla :subplan pa} b]
      {:plan-type :dup :data-lens dla :subplan (merge-plan pa b)}
      [a {:plan-type :dup :data-lens dlb :subplan pb}]
      {:plan-type :dup :data-lens dlb :subplan (merge-plan pb a)}
      
      [{:plan-type :comp :subplans spa} {:plan-type :comp :subplans spb}]
      {:plan-type :comp
       :subplans (reduce-kv (fn [sp src-lensb pb]
                              (if-let [[old-lens new-lens merged-plan]
                                       (some 
                                         (fn [[src-lensa pa]]
                                           (match [(lens/gcl src-lensa src-lensb)]
                                             [[[] ra rb]] nil
                                             [[common ra rb]] [src-lensa common (merge-plan {:plan-type :comp :subplans {(vec ra) pa}}
                                                                                  {:plan-type :comp :subplans {(vec rb) pb}})]
                                             :else (throw (ex-info "Conflicting subplans" {:subplans {src-lensa pa src-lensb pb}}))))
                                         sp)]
                                (-> sp (dissoc old-lens) (assoc new-lens merged-plan))
                                (-> sp (assoc src-lensb pb))))
                  spa spb)}
      
      :else (if (= a b)
                  a 
                  (throw (ex-info "Conflicting plans" {:plans [a b]})))))))

(defn planify 
  "Turns one rule into a plan."
  [scope-index]
  (fn [rule]
    {:plan-type :comp
     :subplans {(:src-lens rule) 
                (case (:rule-type rule)
                  :set {:plan-type :set
                        :data-lens [scope-index (:data-lens rule)]}
                  :dup {:plan-type :dup
                        :data-lens [scope-index (:data-lens rule)]
                        :subplan (reduce merge-plan (map (planify (inc scope-index)) (:subrules rule)))})}}))

(defn plan
  "Turns a set of rules into a plan." [rules]
  (transduce (map (planify 0)) merge-plan rules))

(defn- disjoint-lens-order
  [[a] [b]]
  (if-let [lo-a (cond (number? a) a (lens/slice? a) (:from a))]
    (if-let [lo-b (cond (number? b) b (lens/slice? b) (:from b))]
      (- lo-b lo-a)
      -1)
    (if (or (number? b) (lens/slice? b))
      1
      0)))

(defn exec* [src plan datas]
  (case (:plan-type plan)
    :set (lens/fetch datas (:data-lens plan))
    :dup (mapcat #(exec* src (:subplan plan) (conj datas %)) (lens/fetch datas (:data-lens plan)))
    :comp (reduce (fn [src [src-lens subplan]]
                    (lens/update src src-lens exec* subplan datas))
            src (sort-by key disjoint-lens-order (:subplans plan)))))

(defn exec [src plan data]
  (exec* src plan [data]))


