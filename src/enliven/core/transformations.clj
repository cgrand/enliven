(ns enliven.core.transformations
  (:require [enliven.core.actions :as action]
    [enliven.core.grounder :as grounder]
    [enliven.core.selectors :as sel]
    [enliven.core.locs :as loc]
    [enliven.core.segments :as seg]
    [enliven.core.paths :as path]))

(defn composite [transformations]
  (fn [loc] (mapcat #(% loc) transformations)))

(defn mash [& transformations]
  (composite transformations))

(defn at* 
  ([selector+transformations] (at* selector+transformations identity))
  ([selector+transformations sel]
    (if (next selector+transformations)
      (composite
        (for [[selector t] (partition 2 selector+transformations)
              :let [selector (sel selector)]]
          (fn [loc] (mapcat t (selector loc)))))
      (first selector+transformations))))

(defn at [& selector+transformations]
  (at* selector+transformations))

(defn replace 
  ([path]
    (let [action (action/replace path)]
      (fn [loc]
        (let [seg (:seg loc)
              sloc (if (= 0 seg) ; canonical paths can't have non-zero numeric segments
                     (loc/up loc)
                     loc)]
          [[(path/canonical (loc/path sloc)) action]]))))
  ([selector path]
    (at selector  (replace path))))

(defn dup [path sub]
  (let [action (action/dup path sub)]
    (fn [loc]
      (let [seg (:seg loc)
            sloc (cond
                   (= 0 seg) ; canonical paths can't have non-zero numeric segments
                   (loc/up loc)
                   (seg/slice? seg)
                   loc
                   :else (throw (ex-info "Unexpected location for a dup"
                                  {:loc loc :action action})))
            nloc (-> sloc loc/node loc/loc)
            nloc (if (= loc sloc)
                   nloc
                   (loc/down nloc seg))]
        [[(path/canonical (loc/path sloc)) (action/update action :subs
                                             grounder/ground-loc nloc)]]))))

(defn if' [path then-sub else-sub]
  (let [action (action/if' path then-sub else-sub)]
    (fn [loc]
      (let [seg (:seg loc)
            sloc (if (= 0 seg) ; canonical paths can't have non-zero numeric segments
                   (loc/up loc)
                   loc)
            nloc (-> sloc loc/node loc/loc)
            nloc (if (= loc sloc)
                   nloc
                   (loc/down nloc seg))]
        [[(path/canonical (loc/path sloc)) (action/update action :subs
                                             grounder/ground-loc nloc)]]))))