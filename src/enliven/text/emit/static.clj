(ns enliven.text.emit.static
  (:require [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.segments :as seg]
    [enliven.commons.emit.static :as static]))

(defmethod static/prerender :enliven.text/char [node-type text plan emit acc]
  (if (char? text)
    (if (nil? plan)
      (emit acc text)
      (static/prerender-unknown plan :enliven.text/char emit acc))
    (static/prerender :enliven.text/chars text plan emit acc)))

(defmethod static/prerender :enliven.text/chars [node-type text plan emit acc]
  (static/prerender-nodes text plan :enliven.text/char emit acc))
