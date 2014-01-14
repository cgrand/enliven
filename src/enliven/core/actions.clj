(ns enliven.core.actions)

(def discard [::discard 0 nil])
(defn replace [path] [::replace 0 [path]])
(defn dup [path sub]  [::dup 0 [path] sub])

(defmulti update-subs (fn [[op] f & args] op))

(defmethod update-subs :default [action f & args] action)

(defmethod update-subs ::if [action f & args] 
  (assoc action
    3 (apply f (nth action 3) args)
    4 (apply f (nth action 4) args)))

(defmethod update-subs ::dup [action f & args] 
  (assoc action 3 (apply f (nth action 3) args)))
