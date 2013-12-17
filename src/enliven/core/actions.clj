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

(defmulti perform (fn [[op] node stack exec] op))

(defmethod perform ::replace [[op n [path]] node stack exec]
  (-> stack (nth n) (get-in path)))

(defmethod perform ::discard [[op n [path]] node stack exec]
  nil) ; should return a ::tombstone?

(defmethod perform ::if [[op n [path] then else] node stack exec]
  (if (-> stack (nth n) (get-in path))
    (exec then node stack)
    (exec else node stack)))

(defmethod perform ::dup [[op n [path] sub] node stack exec]
  (for [item (-> stack (nth n) (get-in path))
        :let [stack (conj stack item)]]
    (exec sub node stack)))