(ns enliven.html
  (:require [enliven.core.actions :as action]
    [enliven.core.segments :as seg]
    [enliven.core.locs :as loc]
    [enliven.core.selectors :as sel]))

(seg/defseg ::classes
  (fn [s _]
    (zipmap (re-seq #"\S+" s) (constantly true)))
  (fn [s _ classes]
    (reduce-kv (fn [^StringBuilder sb k v]
                 (if v (.append sb k) sb))
      (StringBuilder.) classes)
    (zipmap (re-seq #"\S+" s) (constantly true))))

(defn class [selector & class+paths]
  (for [[class path] (partition 2 class+paths)]
    [(sel/chain selector (sel/by-path [:attrs :class ::classes class]))
     (action/replace path)]))

(defn class [selector & attr+paths]
  (for [[attr path] (partition 2 attr+paths)]
    [(sel/chain selector (sel/by-path [:attrs attr]))
     (action/replace path)]))

(defn content [selector path]
  [[(sel/chain selector (sel/by-path [:content])) (action/replace path)]])

(defn prepend [selector path]
  [[(sel/chain selector (sel/by-path [:content] [0 0])) (action/replace path)]])

(defn append [selector path]
  [[(sel/chain selector (sel/by-path [:content] [java.lang.Long/MAX_VALUE java.lang.Long/MAX_VALUE])) (action/replace path)]])

(defn dup [selector path & directives]
  [[selector (action/dup path (reduce into #{} directives))]])

(defn if [selector test then else]
  [[selector [::action/if 0 [test] then else]]])

(defn discard [selector]
  [[selector action/discard]])
