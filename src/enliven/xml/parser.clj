;   Copyright (c) Christophe Grand. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns enliven.xml.parser
  (:require [clojure.zip :as z])
  (:import (org.xml.sax ContentHandler Attributes SAXException XMLReader)
           (org.xml.sax.ext DefaultHandler2)
           (javax.xml.parsers SAXParser SAXParserFactory)))

(defrecord Element [tag attrs content])

(def tag? :tag)
(defn- document? 
  "Document nodes are a parsing implementation details and should never leak
   outside of it."
  [x] (= :document (:type x)))

(defn comment? [x] (= :comment (:type x)))
(defn dtd? [x] (= :dtd (:type x)))

(defn xml-zip 
 "Returns a zipper for xml elements (as from xml/parse),
 given a root element"
 [root]
   (z/zipper #(or (tag? %) (document? %)) 
     (comp seq :content) #(assoc %1 :content %2) root))

(defn- insert-element [loc e]
  (-> loc (z/append-child e) z/down z/rightmost))

(defn- merge-text-left [loc s]
  (or
    (when-let [l (-> loc z/down z/rightmost)]
      (when (-> l z/node string?)
        (-> l (z/edit str s) z/up)))
    (-> loc (z/append-child s)))) 

(defn- update-loc [[ns-maps loc] f & args]
  [ns-maps (apply f loc args)])

(defn- assoc-mapping [ns-map prefix uri]
  (assoc ns-map uri (conj (get ns-map uri #{}) prefix)))

(defn- handler [state metadata]
  (proxy [DefaultHandler2] []
    (startElement [uri local-name q-name ^Attributes atts]
     (swap! state
       (fn [[ns-maps loc]]
         (let [e (Element. 
                   (keyword local-name)
                   (when (pos? (. atts (getLength)))
                     (reduce #(assoc %1 (let [attr (keyword (.getLocalName atts %2))]
                                          (if-let [uri (.getURI atts %2)]
                                            (with-meta [attr uri] {::ns (peek ns-maps)
                                                                   ::prefix (re-find #"^[^:]+" (.getQName atts %2))})
                                            attr)) (.getValue atts (int %2))) 
                       {} (range (.getLength atts))))
                   []
                   {::ns (peek ns-maps)}
                   nil)]
           [ns-maps (insert-element loc e)]))))
    (startPrefixMapping [prefix uri]
      (swap! state (fn [[ns-maps loc]]
                     [(conj ns-maps (assoc-mapping (peek ns-maps) prefix uri)) loc])))
    (endPrefixMapping [prefix]
      (swap! state (fn [[ns-maps loc]] [(pop ns-maps) loc])))
    (endElement [uri local-name q-name]
      (prn 'endElement [uri local-name q-name])
      (swap! state update-loc z/up))
    (characters [ch start length]
      (swap! state update-loc merge-text-left (String. ^chars ch (int start) (int length))))
    (ignorableWhitespace [ch start length]
      (swap! state update-loc merge-text-left (String. ^chars ch (int start) (int length))))
    (comment [ch start length]
      (swap! state update-loc z/append-child {:type :comment :data (String. ^chars ch (int start) (int length))}))
    (startDTD [name publicId systemId]
      (swap! state update-loc z/append-child {:type :dtd :data [name publicId systemId]}))
    (resolveEntity
      ([name publicId baseURI systemId]
        (doto (org.xml.sax.InputSource.)
          (.setSystemId systemId)
          (.setPublicId publicId)
          (.setCharacterStream (java.io.StringReader. "")))) 
      ([publicId systemId]
        (let [^DefaultHandler2 this this]
          (proxy-super resolveEntity publicId systemId))))))

(defn startparse-sax [s ch]
  (-> (SAXParserFactory/newInstance)
   (doto
     (.setValidating false)
     (.setFeature "http://xml.org/sax/features/external-general-entities" false)
     (.setFeature "http://xml.org/sax/features/external-parameter-entities" false)
     (.setFeature "http://xml.org/sax/features/namespaces" true)) 
   .newSAXParser
   (doto
     (.setProperty "http://xml.org/sax/properties/lexical-handler" ch))
   (.parse s ch)))

(defn parse
  "Parses and loads the source s, which can be a File, InputStream or
  String naming a URI. Returns a seq of tree of the xml/element struct-map,
  which has the keys :tag, :attrs, and :content. and accessor fns tag,
  attrs, and content. Other parsers can be supplied by passing
  startparse, a fn taking a source and a ContentHandler and returning
  a parser"
  ([s] (parse s startparse-sax))
  ([s startparse]
    (let [state (atom [() (-> {:type :document :content nil} xml-zip)])
          metadata (atom {})
          content-handler (handler state metadata)]
      (startparse s content-handler)
      (map #(if (instance? clojure.lang.IObj %) (vary-meta % merge @metadata) %)
        (-> @state second z/root :content)))))
         
