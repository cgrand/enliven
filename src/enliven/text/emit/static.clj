(ns enliven.text.emit.static
  (:require [enliven.core.actions :as action]
    [enliven.core.plans :as plan]
    [enliven.core.segments :as seg]
    [enliven.commons.emit.static :as static]))

(defn prerender-text [text plan emit acc]
  (if (char? text)
    (if (nil? plan)
      (emit acc text)
      (static/prerender-unknown plan prerender-text emit acc))
    (static/prerender-fragment text plan prerender-text emit acc)))
