(ns enliven.html.util)

(defn ord-hash [contents]
  (let [contents (seq contents)]
    (reify                   
      clojure.lang.IPersistentMap
      (assoc [_ k v]
        (ord-hash (cons [k v] (filter (fn [[ky _]] (not= k ky)) contents))))
      (assocEx [_ k v]
        (if (some (fn [[ky _]] (= k ky)) contents)
          (throw (Exception. ("Key already present"))) 
          (ord-hash (cons [k v] contents))))
      (without [_ k]
        (ord-hash (filter (fn [[ky _]] (not= k ky)) contents)))

      clojure.lang.ILookup
      (valAt [_ k] (some (fn [[ky _]] (= k ky)) contents))
      (valAt [_ k not-found] (or (some (fn [[ky _]] (= k ky)) contents)
                                 not-found))
      clojure.lang.Seqable
      (seq [_]
        (.seq contents))
      
      java.lang.Iterable
      (iterator [this]
        (.iterator contents))
      
      clojure.lang.Associative
      (containsKey [_ k]
        (.containsKey (boolean (some (fn [[ky _]] (= k ky)) contents))))
      (entryAt [_ k]
        (.entryAt (let [el (some (fn [[ky _]] (= k ky)) contents)]
                    (when el (second el)))))
      
      clojure.lang.Counted
      (count [_]
        (.count contents)))))
