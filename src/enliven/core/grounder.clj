(ns enliven.core.grounder
  (:require [enliven.core.rules :as rules]
    [enliven.core.actions :as action]
    [enliven.core.selectors :as sel]
    [enliven.core.locs :as loc]))

;; a transformation is a function from loc to seq of rules
(defn- ground-transformation [transformation node]
  (transformation (loc/loc node)))

(defn ground [transformations node]
  (reduce rules/conj-rule rules/id
    (mapcat #(ground-transformation % node) transformations)))

(defn simple-transformation [selector action]
  (fn [loc]
    (for [loc (selector loc)]
      [(loc/path loc) (action/update-subs action ground-transformation (loc/node loc))])))

(defn composite-transformation [transformations]
  (fn [loc] (mapcat #(% loc) transformations)))