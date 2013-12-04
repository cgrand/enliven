(ns enliven.paths)

(def ^:private contexts-grammar
  {:clojure.xml/node 
   {:tag ...
    :content :clojure.xml/nodes
    :attrs :clojure.xml/attrs}
   :clojure.xml/nodes {::default :clojure.xml/node}
   :clojure.xml/attrs {::default :clojure.xml/attr-value}
   :clojure.xml/attr-value 
   {:enliven.html/classes :enliven.html/class}})

(defn getter [ctx path]
  (reduce
    (fn [[ctx []] p]
      (let [m (contexts-grammar ctx)]
        (if-let [ctx (or (get m p) (get m ::default))]
          [ctx XXX]
          (throw (ex-info "Unsupported segment" 
                   {:segment p :context ctx})))))
    [ctx []] path))

(defn segments
  "Returns the segments out of x in a given context as
   a map of segments to contexts."
  [x ctx]
  ...)

(defn traverse
  "Returns [value ctx]."
  [x ctx path]
  (reduce (fn [[x ctx] segment]
            ((contexts ctx) x segment))
    [x ctx] path))

(defmacro defsegment [name [from to]])

(defsegment :classes ::attr ::classes)

(defsegment string? ::classes ::class)

[::node :content ::nodes
 ::nodes number? ::node
 ::nodes vector? ::nodes
 ::node :attrs ::attrs
 ::attrs keyword? ::attr
 ::attr :classes ::classes
 ::]

(extend-transitions
  {::node {:content ::nodes}})

(defn extend-transitions [m]
  )

(defn getter [ctx path]
  (reductions))