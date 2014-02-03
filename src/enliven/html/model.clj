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


(seg/defsegment append-modified-pairs  ; better name wanted
  "takes sequence of key value pairs in order and presents a
   hash back where modified items are append to the chain of
   key value pairs.
   Example:
   #(let [in [[:a 1] [:b 2] [:c 3]]]
      (-> in
          (fetch append-modified-pairs)
          (assoc :a 5)
          (assoc :d 6)
          (#(putback in append-modified-pairs %)))
   # [[:b 2] [:c 3] [:d 6] [:a 5]] "
  [pairs hash]
  :fetch (into {} pairs)
  :putback (loop [kvs [] okvs pairs m hash]
             (if-let [[[k v :as kv] & okvs] okvs]
               (if (= (get m k m) v)
                 (recur (conj kvs kv) okvs (dissoc m k))
                 (recur kvs okvs m))
               (into kvs m))))


(seg/defsegment styles
  "allows you to operate on seqence of key value pairs vs the
   style attribute string directly"
  [style-attr styles]
  :fetch
    (read-css-attributes style-attr)
  :putback
    (reduce (fn [s [k v]] (str s k ":" v ";")) "" styles))



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

