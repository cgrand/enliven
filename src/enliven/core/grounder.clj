(ns enliven.core.grounder
  (:require [enliven.core.actions :as action]
    [enliven.core.locs :as loc]
    [enliven.core.lenses :as lens]))

;; a transformation is a function from loc to seq of [loc action]
;; rules (as returned by ground-loc) are seq of [path action]
(defn ground-loc
  ([transformation loc]
    (ground-loc transformation loc (loc/path loc)))
  ([transformation loc prefix-path]
    (for [[sloc action] (transformation loc)]
      [(lens/rm-prefix (loc/path sloc) prefix-path) action])))

(defn ground
  "Returns a seq of [path action]."
  [transformation node]
  (ground-loc transformation (loc/loc node)))
