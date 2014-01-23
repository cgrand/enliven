(ns enliven.core.actions)

(defn replace [path] {:op ::replace :scope-idx 0 :args [path]})
(defn dup [path sub]  {:op ::dup :scope-idx 0 :args [path] :subs [sub]})
(defn if' [path then-sub else-sub]
  {:op ::dup :scope-idx 0 :args [path] :subs [then-sub else-sub]})

(defn update [action key f & args]
  (assoc action key (mapv #(apply f % args) (get action key))))
