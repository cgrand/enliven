(ns enliven.core.actions
  (:refer-clojure :exclude [replace]))

;; An action is made of an op, a scope-idx, an arg and

(defn replace [path] {:op ::replace :scope-idx 0 :arg path})
(defn dup [path sub]  {:op ::dup :scope-idx 0 :arg path :subs [[list sub]]})

(defmulti update (fn [action key f & args] key))

(defmethod update :default [action key f & args]
  (assoc action key (mapv #(apply f % args) (get action key))))

#_(defmethod update :subs [action key f & args]
   (assoc action :subs (mapv (fn [[p sub]] [p (apply f sub args)]) (:subs action))))

(defmethod update :arg [action key f & args]
  (if-let [path (:arg action)]
    (assoc action :arg (apply f path args))
    action))
