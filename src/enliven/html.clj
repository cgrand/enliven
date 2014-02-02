(ns enliven.html
  (:refer-clojure :exclude [class descendants])
  (:require [enliven.core.actions :as action]
    [enliven.html.model :as html]
    [enliven.core.segments :as seg]
    [enliven.core.locs :as loc]
    [enliven.core.selectors :as sel]
    [enliven.core.grounder :as grounder]
    [enliven.core.transformations :as transform]
    [enliven.core.plans :as plan]
    enliven.html.emit.static
    [enliven.commons.emit.static :as static]
    [clojure.string :as str]))

(defn children [loc]
  (when (:tag (loc/node loc))
    (let [loc (loc/down loc :content)]
      (for [i (range (count (loc/node loc)))]
        (loc/down loc i)))))

(defn text-node [loc]
  (when (string? (loc/node loc)) (list loc)))

(defn element [loc]
  (let [node (loc/node loc)]
    (when (and (map? node) (:tag node))
      (list loc))))

(defn descendants [loc]
  (mapcat #(cons % (descendants %)) (children loc)))

(defn descendants-and-self [loc]
  (cons loc (descendants loc)))

(defn indirect-adjacent-combinator
  "http://www.w3.org/TR/2001/CR-css3-selectors-20011113/#adjacent-i-combinators"
  [loc]
  (sel/chain (sel/rights loc) element))

(defn direct-adjacent-combinator
  "http://www.w3.org/TR/2001/CR-css3-selectors-20011113/#adjacent-d-combinators"
  [loc]
  (some-> loc indirect-adjacent-combinator first list))

(defn loose-chain
  ([] list)
  ([& sels] (reduce sel/chain (interpose descendants sels))))

(defprotocol ^:private ToSelector
  (as-sel [x]))

(def ^:private css-combinators
  {com.phloc.css.decl.ECSSSelectorCombinator/BLANK descendants
   com.phloc.css.decl.ECSSSelectorCombinator/TILDE indirect-adjacent-combinator
   com.phloc.css.decl.ECSSSelectorCombinator/PLUS direct-adjacent-combinator
   com.phloc.css.decl.ECSSSelectorCombinator/GREATER list})

(defn- css-sel [^com.phloc.css.decl.CSSSelector sel]
  (reduce sel/chain
    list (map #(or (css-combinators %) (as-sel %)) (.getAllMembers sel))))

(defn- css-members-sel [members]
  (->> members (map css-sel) (apply sel/union)))

(defn css [^String s]
  (-> s
    (str " {}")
    (com.phloc.css.reader.CSSReader/readFromString
      "utf-8"
      com.phloc.css.ECSSVersion/CSS30)
    .getAllStyleRules
    ^com.phloc.css.decl.CSSStyleRule first
    .getAllSelectors
    css-members-sel))

(defn sel
  ([spec]
    (sel spec :root))
  ([spec relationship]
    (if (sequential? spec)
      (first (reduce
               (fn [[r relationship] step]
                 (if (= :> step)
                   [r (case relationship
                        (:strict-root :root) :strict-root
                        :children)]
                   [(sel/chain r (sel step relationship)) :descendants]))
              [list relationship] spec))
      (sel/chain
        (case relationship
          :strict-root list
          :root descendants-and-self
          :children children
          :descendants descendants)
        (cond
          (string? spec) (css spec)
          (keyword? spec) (css (name spec))
          :else spec)))))

(defn tag-selector [tag]
  (sel/node-pred #(some->> % :tag (= tag))))

(defn- space-separated-values [s]
  (re-seq #"\S+" s))

(defn class-selector [class]
  (sel/node-pred #(some->> % :attrs :class space-separated-values (some #{class}))))

(defn id-selector [id]
  (sel/node-pred #(some-> % :attrs :id str/trim (= id))))

(extend-protocol ToSelector
  com.phloc.css.decl.CSSSelectorSimpleMember
  (as-sel [sel]
    (let [s (.getValue sel)]
      (case (first s)
        \# (id-selector (subs s 1))
        \. (class-selector (subs s 1))
        \: (throw (ex-info "unsupported pseudo-class" {:pseudo-class s}))
        (tag-selector (keyword s)))))
  com.phloc.css.decl.CSSSelectorAttribute
  (as-sel [sel]
    (let [attr (keyword (.getAttrName sel))
          value (.getAttrValue sel)
          op (.getOperator sel)
          attr-pred
          (condp = op
            nil identity
            com.phloc.css.decl.ECSSAttributeOperator/EQUALS
            #(= % value)
          com.phloc.css.decl.ECSSAttributeOperator/INCLUDES
          #(some->> % space-separated-values (some #{value}))
          com.phloc.css.decl.ECSSAttributeOperator/DASHMATCH
          (let [value- (str value "-")]
            (fn [^String v] (or (= v value) (.startsWith v value-))))
          com.phloc.css.decl.ECSSAttributeOperator/BEGINMATCH
          #(.startsWith ^String % value)
          com.phloc.css.decl.ECSSAttributeOperator/CONTAINSMATCH
          #(>= (.indexOf ^String % value) 0)
          com.phloc.css.decl.ECSSAttributeOperator/ENDMATCH
          #(.endsWith ^String % value)
          (throw (ex-info "Unexpected attribute operator" {:op op})))]
      (sel/node-pred
        #(some-> % :attrs attr attr-pred))))
  #_#_com.phloc.css.decl.CSSSelectorMemberNot
  (as-sel [sel]
    (let [nested-selector (-> sel .getNestedMembers css-members-sel)]
      (fn [loc]))))

(defn at [& selector+transformations]
  (transform/at* selector+transformations sel))

;; html-specific transformations
(defn class
  "Set a class (on the selected elements) when the value at the corresponding path in the model is true."
  {:arglists '([class path & class+paths])}
  [& class+paths]
  (transform/composite
    (for [[class path] (partition 2 class+paths)]
      (transform/replace
        (sel/chain element (sel/by-path [:attrs :class html/classes (name class)]))
        path))))

(defn style
  "set style attributes (on the selected elements) to the value at the corriesponding path in the model. This transform will append modified styles to the end of style string. If you have multiple pairs being changes it does not guarantee order of appended pairs."
  {:arglists '([style path & syle+paths])}
  [& style+paths]
  (transform/composite
    (for [[style path] (partition 2 style+paths)]
      (transform/replace
        (sel/chain element (sel/by-path [:attrs :style html/styles html/append-modified-pairs (name style)]))
        path))))

(defn attr
  "Set an attribute (on the selected elements) to the value at the corresponding path in the model."
  {:arglists '([attr path & attr+paths])}
  [& attr+paths]
  (transform/composite
    (for [[attr path] (partition 2 attr+paths)]
      (transform/replace
        (sel/chain element (sel/by-path [:attrs (keyword attr)]))
        path))))

(defn content
  "Set the content (of the selected elements) the value at the path in the model."
  [path]
  (transform/replace
    (sel/chain element (sel/by-path [:content (seg/slice 0 java.lang.Long/MAX_VALUE)]))
    path))

(defn prepend [path]
  (transform/replace
    (sel/chain element (sel/by-path [:content (seg/slice 0 0)]))
    path))

(defn append [path]
  (transform/replace
    (sel/chain element (sel/by-path [:content (seg/slice java.lang.Long/MAX_VALUE java.lang.Long/MAX_VALUE)]))
    path))

(defn dup
  "Dup[licate] the selected nodes for each item in the collection at the path in the model.
  Each copy is then transformed using the specified transformations.
  For these transformatons the model is restricted to the item."
  [path & transformations]
  (transform/dup path (apply at transformations)))

#_(defn if [selector test then else]
   [[selector [::action/if 0 [test] then else]]])

(def discard (dup (seg/const nil)))

(defn static-template [node & transformations]
  (let [plan (plan/plan (grounder/ground (apply at transformations) node))
        [node plan] (plan/const-execute node plan (list ::plan/dynamic))
        plan (plan/tidy plan)
        emitted (static/tight-fn-emit! (static/prerender ::html/node node plan identity static/tight-fn-emit! (static/tight-fn-emit!)))]
    (fn
      ([data] (static/render emitted data))
      ([data emit acc] (static/render emitted data emit acc)))))

#_(defn static-bytes-template [node & transformations]
   (let [plan (plan/plan (grounder/ground (apply at transformations) node))
         [node plan] (plan/const-execute node plan (list ::plan/dynamic))
         plan (plan/tidy plan)
         emit static/bytes-fn-emit!
         emitted (emit (static/prerender ::html/node node plan emit (emit)))]
     (fn
       ([data] (.toByteArray
                 ^java.io.ByteArrayOutputStream (static/render emitted data
                                                  (fn [^java.io.ByteArrayOutputStream out ^bytes x]
                                                    (doto out (.write x 0 (alength x))))
                                                  (java.io.ByteArrayOutputStream.))))
       ([data emit acc] (static/render emitted data emit acc)))))
