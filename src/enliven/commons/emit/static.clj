(ns enliven.commons.emit.static)

(defn- flat-reduce [f acc x]
  (if (or (fn? x) (string? x))
    (f acc x)
    (reduce (partial flat-reduce f) acc x)))

(defn tighten [x]
  (flat-reduce (fn [v x]
                 (if (and (string? (peek v)) (string? x))
                   (conj (pop v) (str (peek v) x))
                   (conj v x)))
    [] x))

(defn render* 
  "Similar to render but takes a stack of scopes instead of just the model."
  [emitted stack f acc]
  (cond 
    (fn? emitted) (emitted f acc stack)
    (string? emitted) (f acc emitted)
    (char? emitted) (f acc (str emitted))
    :else (reduce #(render* %2 stack f %1) acc emitted)))

(defn render
  "Renders a compiled (emitted) template. Either as a string or using a reduce-like interface."
  ([emitted data]
    (str (render emitted data (fn [^StringBuilder sb s] (.append sb s))
          (StringBuilder.))))
  ([emitted data f acc]
    (render* emitted (list data) f acc)))