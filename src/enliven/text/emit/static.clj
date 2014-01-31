(ns enliven.text.emit.static
  (:require [enliven.text.model :as text]
    [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.segments :as seg]
    [enliven.commons.emit.static :as static]))

(defmethod static/prerenderer-fn ::text/char [node-type]
  (fn [text plan enc emit acc]
    (if (char? text)
      (if (nil? plan)
        (emit acc (enc (str text)))
        (static/prerender-unknown plan ::text/char (comp enc str) emit acc))
      (static/prerender ::text/chars text plan enc emit acc))))

(defmethod static/prerenderer-fn ::text/chars [node-type]
  (fn [text plan enc emit acc]
    (static/prerender-nodes text plan ::text/char enc emit acc)))
