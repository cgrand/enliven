(ns enliven.text.model
  (:refer-clojure :exclude [chars])
  (:require [enliven.core.segments :as seg]))

(seg/defsegment chars [s cs]
  :fetch (vec s)
  :putback (apply str cs))

(seg/deftransitions
  {::chars {`chars ::chars
            `seg/slice ::chars
            Number ::char}})
