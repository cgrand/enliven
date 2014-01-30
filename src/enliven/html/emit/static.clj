(ns enliven.html.emit.static
  (:require [enliven.html.model :as html]
    [enliven.text.model :as text]
    [enliven.core.segments :as seg]
    [enliven.commons.emit.static :as static]
    [enliven.core.plans :as plan]
    [enliven.core.actions :as action]))

(defn known-segs-only? [plan known-segs]
  (every? known-segs (keys plan)))

(defn escape-text-node [text-node]
  (-> text-node str (.replace "&" "&amp;") (.replace "<" "&lt;")))

(defn escape-attr-value
  "Escape string for use in an attribute. Avoid escaping ampersands as much as possible.
   (see HTML5 parsing algorithm)"
  [^String attr-value]
  (-> (re-matcher #"&([a-zA-Z0-9]+(?![=a-zA-Z0-9])|#)" attr-value)
    (.replaceAll "&amp;$1")
    (.replace "'" "&quot;")))

(defn prerender-text-node [node plan emit acc]
  (if plan
    (let [enc-emit (static/compose-encoding emit escape-text-node)]
      (if-let [char-plan (some-> plan :misc (get text/chars))]
        (static/prerender ::text/chars
          (seg/fetch node text/chars)
          char-plan
          enc-emit acc)
        (static/prerender-unknown node plan ::html/node enc-emit acc)))
    (emit acc (escape-text-node node))))

(defmethod static/prerenderer-fn ::html/tag [node-type]
  (fn [tag plan emit acc]
    (if plan
      (static/prerender-unknown tag plan node-type emit acc)
      (emit acc (name tag)))))

(defmacro ^:private inline-emit
  "Threads the emit fn and its accumulator through each items of coll.
  When the item is unquoted, emit and acc are apssed as the last two arguments."
  [emit acc & coll]
  (let [emitsym (gensym 'emit)]
    `(let [~emitsym ~emit]
       ~(reduce (fn [acc x]
                  (if (and (seq? x) (= `clojure.core/unquote (first x)))
                    (let [expr (second x)]
                      (concat (if (seq? expr) expr (list expr)) [emitsym acc]))
                    (list emitsym acc x)))
          acc coll))))

(defn- render-attrs [attrs emit acc]
  (reduce-kv (fn [acc attr v]
               (cond 
                 (true? v) (emit acc (name attr)) 
                 v (inline-emit emit acc " " (name attr) 
                     "='" (escape-attr-value v) "'")
                 :else acc))
    acc attrs))

(defmethod static/prerenderer-fn ::html/attrs [node-type]
  (fn [attrs plan emit acc]
    (cond
      (nil? plan) (render-attrs attrs emit acc)
      (or (:action plan) (not-every? keyword? (keys (:misc plan))))
        (static/prerender-unknown attrs plan node-type emit acc)
      :else
      (let [untoucheds (reduce dissoc attrs (keys (:misc plan)))
            toucheds (reduce dissoc attrs (keys untoucheds))]
        (inline-emit emit acc
          ~(render-attrs untoucheds)
          ~(static/prerender-unknown toucheds plan node-type))))))

(defn prerender-element [node plan emit acc]
  (if (and (not (:action plan)) (known-segs-only? (:misc plan) #{:tag :attrs :content}))
    (let [plan-by-seg (:misc plan)
          prerender (fn [seg emit acc]
                      (static/prerender (seg/fetch-type ::html/node seg) (seg/fetch node seg) (get plan-by-seg seg)
                        emit acc))]
      (inline-emit emit acc
        "<" ~(prerender :tag) ~(prerender :attrs) ">" ~(prerender :content) "</" ~(prerender :tag) ">"))
    (static/prerender-unknown node plan ::html/node emit acc)))

(defmethod static/prerenderer-fn ::html/node [node-type]
  (fn [node plan emit acc]
    (cond
      (string? node) (prerender-text-node node plan emit acc)
      (:tag node) (prerender-element node plan emit acc)
      (or (nil? node) (sequential? node)) (static/prerender ::html/nodes node plan emit acc)
      :else (throw (ex-info "Unexpected node" {:node node})))))

(defmethod static/prerenderer-fn ::html/nodes [node-type]
  (fn [nodes plan emit acc]
    (static/prerender-nodes nodes plan ::html/node emit acc)))
