(ns enliven.core.grounder
  (:require [enliven.core.actions :as action]
    [enliven.core.locs :as loc]
    [enliven.core.paths :as path]))

;; a transformation is a function from loc to seq of rules
(defn ground-loc [transformation loc]
  (for [[path action] (transformation loc)]
    [(path/canonical path) (action/update action :args path/canonical)]))

(defn ground [transformation node]
  (ground-loc transformation (loc/loc node)))
