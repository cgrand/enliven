(ns enliven.core.grounder
  (:require [enliven.core.rules :as rules]
    [enliven.core.actions :as action]
    [enliven.core.selectors :as sel]
    [enliven.core.locs :as loc]))

(declare ground)

(defn- ground-directive [[selector action] node]
  (for [loc (sel/locs node selector)]
    [(loc/path loc) (action/update-subs action ground (loc/node loc))]))

(defn ground [directives node]
  (reduce rules/conj-rule rules/id
    (mapcat #(ground-directive % node) directives)))
