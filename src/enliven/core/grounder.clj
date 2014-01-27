(ns enliven.core.grounder
  (:require [enliven.core.actions :as action]
    [enliven.core.locs :as loc]
    [enliven.core.paths :as path]))

;; a transformation is a function from loc to seq of [loc action]
;; rules (as returned by ground-loc) are seq of [path action]
;; actions are annotated with their nodes.
(defn ground-loc [transformation loc]
  (for [[loc action] (transformation loc)]
    [(path/canonical (loc/path loc)) (-> action
                                       (action/update :args path/canonical)
                                       (assoc :node (loc/node loc)))]))

(defn ground [transformation node]
  (ground-loc transformation (loc/loc node)))
