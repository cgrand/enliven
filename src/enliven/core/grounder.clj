(ns enliven.core.grounder
  (:require [enliven.core.actions :as action]
    [enliven.core.selectors :as sel]
    [enliven.core.locs :as loc]
    [enliven.core.segments :as seg]
    [enliven.core.paths :as path]))

;; a transformation is a function from loc to seq of rules
(defn ground-loc [transformation loc]
  (for [[path action] (transformation loc)]
    [(path/canonical path) (action/update action :args path/canonical)]))

(defn ground
  ([transformation node]
    (ground transformation node nil))
  ([transformation node segs]
    (ground-loc transformation (reduce loc/down (loc/loc node) segs))))

(defn simple-transformation [selector action]
  (fn [loc]
    (for [loc (selector loc)]
      [(path/canonical (loc/path loc)) (action/update action :subs ground (loc/node loc))])))

(defn splice-transformation [selector action]
  (fn [loc]
    (keep (fn [loc]
            (let [seg (:seg loc)
                  [sloc segs] (cond
                                (seg/slice? seg) [loc nil]
                                (zero? seg) ; canonical paths can't have non-zero numeric segments
                                [(loc/up loc) [0]]
                                :else
                                (throw (ex-info "Unexpected location for a splice"
                                         {:loc loc :action action})))]
              [(path/canonical (loc/path sloc)) (action/update action :subs ground (loc/node sloc) segs)]))
      (selector loc))))

(defn composite-transformation [transformations]
  (fn [loc] (mapcat #(% loc) transformations)))

(defn at* 
  ([selector+transformations] (at* selector+transformations identity))
  ([selector+transformations sel]
    (if (next selector+transformations)
      (composite-transformation
        (for [[selector t] (partition 2 selector+transformations)
              :let [selector (sel selector)]]
          (fn [loc] (mapcat t (selector loc)))))
      (first selector+transformations))))

(defn at [& selector+transformations]
  (at* selector+transformations))
