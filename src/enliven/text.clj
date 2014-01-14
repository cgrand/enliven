(ns enliven.text
  (:require [enliven.core.segments :as seg]
    [enliven.core.locs :as loc]
    [enliven.core.actions :as action]
    [enliven.core.grounder :as grounder]))

(seg/defsegment chars [s cs]
  :fetch (vec s)
  :putback (apply str cs))

(defn sel [selector]
  (if (instance? java.util.regex.Pattern selector)
    (fn [loc]
      (let [s (loc/node loc)]
        (when (string? s)
          (let [loc (loc/down loc chars) 
                m (re-matcher selector s)]
            (loop [locs []]
              (if (.find m)
                (recur (conj locs (loc/down loc (seg/slice (.start m) (.end m)))))
                locs))))))
    selector))

(defn replace [selector path]
  (grounder/simple-transformation
    (sel selector) (action/replace path)))

