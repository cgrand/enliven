(ns enliven.text.model
  (:refer-clojure :exclude [chars])
  (:require [enliven.core.lenses :as lens]))

(lens/deflens chars [s cs]
  :fetch (vec s)
  :putback (apply str cs))

(lens/deftransitions
  {::chars {`chars ::chars
            `lens/slice ::chars
            Number ::char}})
