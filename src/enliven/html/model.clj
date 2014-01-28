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
  {::node {:content {:type ::nodes
                     :js/fetcher (fn [node seg]
                                   `(.-childNodes ~node))}
           :attrs {:type ::attrs
                   :js/fetcher (fn [node seg]
                                 `(.-attributes ~node))}
           :tag ::tag
           `text/chars ::text/chars}
   ::nodes {`seg/slice ::nodes
            Number {:type ::node
                    :js/fetcher (fn [node seg]
                                  `(aget ~node ~seg))}}
   ::attrs {clojure.lang.Keyword {:type ::attr-value
                                  :js/fetcher (fn [node seg]
                                                `(aget ~node ~(name seg)))}}
   ::attr-value {`classes ::classes
                 `text/chars ::text/chars}
   ::classes {String ::seg/boolsy}})

