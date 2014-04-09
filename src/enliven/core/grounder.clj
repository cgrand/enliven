(ns enliven.core.grounder
  (:require [enliven.core.actions :as action]
    [enliven.core.locs :as loc]
    [enliven.core.lenses :as lens]))

;; a transformation is a function from loc to seq of [loc action]
;; rules (as returned by ground-loc) are seq of [path action]
(defn ground-loc [transformation loc]
  (let [path (lens/canonical (loc/path loc))]
    (for [[sloc action] (transformation loc)]
     [(lens/relativize (lens/canonical (loc/path sloc)) path)
      (action/update action :arg lens/canonical)])))

(defn ground
  "Returns a seq of [canonical-path action]."
  [transformation node]
  (ground-loc transformation (loc/loc node)))
