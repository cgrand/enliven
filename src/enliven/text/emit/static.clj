(ns enliven.text.emit.static
  (:require [enliven.text.model :as text]
    [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.segments :as seg]
    [enliven.commons.emit.static :as static]))

(defmethod static/prerender ::text/char [node-type text plan emit acc]
  (if (char? text)
    (if (nil? plan)
      (emit acc text)
      (static/prerender-unknown plan ::text/char emit acc))
    (static/prerender ::text/chars text plan emit acc)))

(defmethod static/prerender ::text/chars [node-type text plan emit acc]
  (static/prerender-nodes text plan ::text/char emit acc))
