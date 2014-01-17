(ns enliven.html.emit.static
  (:require [enliven.core.segments :as seg]
    [enliven.commons.emit.static :as static]
    enliven.text enliven.text.emit.static
    [enliven.core.plans :as plan]
    [enliven.core.actions :as action]))

(defn known-segs-only? [plan known-segs]
  (every? known-segs (keys plan)))

(declare prerender)

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
      (if-let [char-plan (some-> plan :misc (get enliven.text/chars))]
        (enliven.text.emit.static/prerender-text
          (seg/fetch node enliven.text/chars)
          char-plan
          enc-emit acc)
        (static/prerender-unknown node plan prerender enc-emit acc)))
    (emit acc (escape-text-node node))))

(defn prerender-tag [tag plan emit acc]
  (if plan
    (static/prerender-unknown tag plan prerender-tag emit acc)
    (emit acc (name tag))))

(defmacro ^:private inline-emit [emit acc & coll]
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

(defn prerender-attrs [attrs plan emit acc]
  (cond
    (nil? plan) (render-attrs attrs emit acc)
    (or (:action plan) (not-every? keyword? (keys (:misc plan))))
      (static/prerender-unknown attrs plan prerender-attrs emit acc)
    :else
    (let [untoucheds (reduce dissoc attrs (keys (:misc plan)))
          toucheds (reduce dissoc attrs (keys untoucheds))]
      (inline-emit emit acc 
        ~(render-attrs untoucheds)
        ~(static/prerender-unknown toucheds plan prerender-attrs)))))

(defn prerender-element [node plan emit acc]
  (if (and (not (:action plan)) (known-segs-only? (:misc plan) #{:tag :attrs :content}))
    (let [plan-by-seg (:misc plan)]
      (inline-emit emit acc
        "<" ~(prerender-tag (:tag node) (:tag plan-by-seg))
         ~(prerender-attrs (:attrs node) (:attrs plan-by-seg))
         ">" ~(prerender (:content node) (:content plan-by-seg))
         "</" ~(prerender-tag (:tag node) (:tag plan-by-seg)) ">"))
    (static/prerender-unknown node plan prerender emit acc)))

(defn prerender
  "During prerendering, strings, chars and fns are expected to be emitted."
  [node plan emit acc]
  (cond
    (string? node) (prerender-text-node node plan emit acc)
    (:tag node) (prerender-element node plan emit acc)
    (sequential? node) (static/prerender-fragment node plan prerender emit acc)
    (nil? node) acc
    :else (throw (ex-info "Unexpected node" {:node node}))))
