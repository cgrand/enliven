(ns enliven.impl.lens
  (:refer-clojure :exclude [juxt]))

;; TODO: explore generalized arrows a little more.

;; Why are paths not enough?
;; Paths would be ok to update the server-side immutable tree,
;; however they are not enough to efficiently update the DOM.
;; When updating the DOM, the meaning of a keyword for example is
;; dependent on the node type.

;; what is really needed is a way to compile a "path" to 
;; an update fn

(defmulti init-updater (fn [type]))

(defmethod init-updater :clj [type] [type []])

(defmulti inc-updater (fn [[type acc] segment] [type segment]))

(defmethod inc-updater :clj [[_ v] segment] [:clj [conj v segment]])

(defmulti finish-updater (fn [[type acc]]))

(defmethod finish-updater :clj [[_ v]] XXX)

(defn updater [type segments]
  (finish-updater (reduce inc-updater (init-updater type) segments)))


;;;;

(defprotocol Lens
  (-focus [lens x])
  (-update [lens x f args]))

(defn focus [x lens]
  (-focus lens x))

(defn update [x lens f & args]
  (-update lens x f args))

(extend-protocol Lens
  clojure.lang.Keyword
  (-focus [k x] (k x))
  (-update [k x f args]
    (assoc x k (apply f (k x) args)))
  Object
  (-focus [o x] (get x o))
  (-update [o x f args]
    (assoc x o (apply f (get x o) args)))
  nil
  (-focus [o x] (get x nil))
  (-update [o x f args]
    (assoc x nil (apply f (get x nil) args))))

(defn juxt [& lenses]
  (reify Lens
    (-focus [lens x]
      (mapv #(-focus % x) lenses))
    (-update [lens x f args]
      (reduce-kv (fn [x i v]
                   (-update (nth lenses i) x (constantly v) nil))
        x (apply f (mapv #(-focus % x) lenses) args)))))

(def id 
  "The identity lens."
  (reify Lens
    (-focus [lens x] x)
    (-update [lens x f args]
      (apply f x args))))

(defn chain 
  ([] id)
  ([lens] lens)
  ([lens & lenses]
    (reify Lens
      (-focus [_ x] (reduce focus (-focus lens x) lenses))
      (-update [_ x f args]
        (letfn [(update-rest 
                  ([x] (apply f x args))
                  ([x lens] (-update lens x f args))
                  ([x lens & lenses] (-update lens x update-rest lenses)))]
          (-update lens x update-rest lenses))))))

