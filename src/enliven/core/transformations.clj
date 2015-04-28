(ns enliven.core.transformations
  (:refer-clojure :exclude [replace])
  (:require [enliven.core.actions :as action]
    [enliven.core.grounder :as grounder]
    [enliven.core.selectors :as sel]
    [enliven.core.locs :as loc]
    [enliven.core.lenses :as lens]))

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
        [[(or (loc/spliceable loc) loc) action]])))
  ([selector path]
    (at selector  (replace path))))

(defn dup [data-path sub]
  (let [action (action/dup data-path sub)]
    (fn [loc]
      (let [sloc (loc/spliceable loc)
            _ (when-not sloc
                (throw (ex-info "Unexpected location for a dup"
                                  {:loc loc :action action})))
            path (loc/path sloc)]
        [[sloc (action/update action :subs
                 (fn [[subsel sub]]
                   (let [subloc (first (subsel loc))]
                     [lens/identity #_(lens/rm-prefix (loc/path subloc) path)
                      (grounder/ground-loc sub subloc path)])))]]))))

#_(defn if' [path then-sub else-sub]
   (let [action (action/if' path then-sub else-sub)]
     (fn [loc]
       (let [sloc (or (loc/spliceable loc) loc)
             nloc (-> sloc loc/node loc/loc)
             nloc (if (= loc sloc)
                    nloc
                    (loc/down nloc 0))]
         [[sloc (action/update action :subs grounder/ground-loc nloc)]]))))
