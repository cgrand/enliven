(ns enliven.text
  (:refer-clojure :exclude [replace])
  (:require [enliven.text.model :as text]
    [enliven.core.lenses :as lens]
    [enliven.core.locs :as loc]
    [enliven.core.actions :as action]
    [enliven.core.transformations :as transform]))

(defn sel [selector]
  (if (instance? java.util.regex.Pattern selector)
    (fn [loc]
      (let [s (loc/node loc)]
        (when (string? s)
          (let [loc (loc/down loc chars)
                m (re-matcher selector s)]
            (loop [locs []]
              (if (.find m)
                (recur (conj locs (loc/down loc (lens/slice (.start m) (.end m)))))
                locs))))))
    selector))

(defn replace [selector path]
  (transform/replace (sel selector) path))

#_(defn static-template [text & transformations]
   (let [plan (plan/plan (grounder/ground (apply at transformations) text))
         emitted (common/tight-fn-emit! (static/prerender node plan common/tight-fn-emit! (common/tight-fn-emit!)))]
     (fn
       ([data] (common/render emitted data))
       ([data emit acc] (common/render emitted data emit acc)))))
