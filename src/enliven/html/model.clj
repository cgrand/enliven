(ns enliven.html.model
  (:require
    [clojure.string :as str]
    [enliven.text.model :as text]
    [enliven.core.lenses :as lens]))

;; html-specific segments
(lens/deflens classes [class-attr classes]
  :fetch
    (zipmap (re-seq #"\S+" (or class-attr "")) (repeat true))
  :putback
    (some->> classes (keep (fn [[k v]] (when v k))) seq (str/join " ")))


(defn read-css-attributes
  "parses an style attribute string into a sequence of key value pairs"
  [attr-str]
  (let [el (com.phloc.css.reader.CSSReaderDeclarationList/readFromString
            (or attr-str "") com.phloc.css.ECSSVersion/CSS30)]
    (if el
      (for [idx (range (.getDeclarationCount el))]
        [(-> (.getDeclarationAtIndex el idx)
             (.getProperty))
         (-> (.getDeclarationAtIndex el idx)
             (.getExpression)
             (.getAsCSSString (com.phloc.css.writer.CSSWriterSettings.
                               com.phloc.css.ECSSVersion/CSS30)
                              0))])
      '())))

(lens/deflens styles
  "allows you to operate on seqence of key value pairs vs the
   style attribute string directly"
  [style-attr styles]
  :fetch
    (read-css-attributes style-attr)
  :putback
    (reduce (fn [s [k v]] (str s k ":" v ";")) "" styles))



(lens/deftransitions
  {::node {:content {:type ::nodes
                     :js/fetcher (fn [node seg]
                                   `(.-childNodes ~node))}
           :attrs {:type ::attrs
                   :js/fetcher (fn [node seg]
                                 `(.-attributes ~node))}
           :tag ::tag
           `text/chars ::text/chars}
   ::nodes {`lens/slice {:type ::nodes
                        :js/fetcher (fn [node seg]
                                      `(nodes-slice ~node ~(:from seg) ~(:to seg)))}
            Number {:type ::node
                    :js/fetcher (fn [node seg]
                                  `(aget ~node ~seg))}
            #_#_:js/replace (fn [node data] ; not the right place
                              `(set! (.-nodeValue ~node) (as-nodes ~data)))}
   ::attrs {clojure.lang.Keyword {:type ::attr-value
                                  :js/fetcher (fn [node seg]
                                                `(aget ~node ~(name seg)))}}
   ::attr-value {`classes ::classes
                 `text/chars ::text/chars
                 `styles ::style-decls
                 #_#_:js/replace (fn [node data] ; not the right place
                                   `(set! (.-nodeValue ~node) ~data))}
   ::style-decls {`lens/append-on-assoc ::style-maps}
   ::classes {String ::lens/boolsy}})

