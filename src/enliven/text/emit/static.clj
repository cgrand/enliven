(ns enliven.text.emit.static
  (:require [enliven.text.model :as text]
    [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.segments :as seg]
    [enliven.commons.emit.static :as static]))

(defmethod static/prerenderer-fn ::text/char [node-type]
  (fn [text plan emit acc]
    (if (char? text)
      (if (nil? plan)
        (emit acc text)
        (static/prerender-unknown plan ::text/char emit acc))
      (static/prerender ::text/chars text plan emit acc))))

(defmethod static/prerenderer-fn ::text/chars [node-type]
  (fn [text plan emit acc]
    (static/prerender-nodes text plan ::text/char emit acc)))
