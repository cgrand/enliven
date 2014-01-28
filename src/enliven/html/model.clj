(ns enliven.html.model
  (:require
    [clojure.string :as str]
    [enliven.text.model :as text]
    [enliven.core.segments :as seg]))

;; html-specific segments
(seg/defsegment classes [class-attr classes]
  :fetch
    (zipmap (re-seq #"\S+" (or class-attr "")) (repeat true))
  :putback
    (some->> classes (keep (fn [[k v]] (when v k))) seq (str/join " ")))

(seg/deftransitions
  {::node {:content ::nodes
           :attrs ::attrs
           :tag ::tag}
   ::nodes {`seg/slice ::nodes
            Number ::node}
   ::attrs {clojure.lang.Keyword ::attr-value}
   ::attr-value {`classes ::classes
                 `text/chars ::text/chars}
   ::classes {String ::seg/boolsy}})

