(ns enliven.html
  (:require [enliven.core.actions :as action]
    [enliven.core.segments :as seg]
    [enliven.core.locs :as loc]
    [enliven.core.selectors :as sel]
    [enliven.core.grounder :as grounder]
    [enliven.core.plans :as plan]
    [enliven.html.emit.static :as static]
    [enliven.commons.emit.static :as common]
    [clojure.string :as str]))

;; html-specific segments
(seg/defsegment classes [class-attr classes]
  :fetch
    (zipmap (re-seq #"\S+" (or class-attr "")) (repeat true))
  :putback
    (some->> classes (keep (fn [[k v]] (when v k))) seq (str/join " ")))


(seg/defsegment styles [style-attr styles]
  :fetch 
    (let [vals (re-seq #"\s*([^:;]*)[:][\s]*([^;]+)"
                       (or style-attr ""))]
      (reduce (fn [m [_ k v]] (assoc m k (.trim v))) {} vals))
  :putback
    (reduce (fn [s [k v]] (str s k ":" v ";")) "" styles ))



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

(defn loose-chain
  ([] list)
  ([& sels] (reduce sel/chain (interpose descendants sels))))

(defprotocol ^:private ToSelector
  (as-sel [x]))

(defn css [^String s]
  (-> (org.apache.batik.css.parser.Parser.)
    (.parseSelectors s)
    as-sel))

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

(defn- tag-is? [tag sel]
  (or (nil? sel) (= tag sel)))

(extend-protocol ToSelector
  org.w3c.css.sac.SelectorList
  (as-sel [sels]
    (let [sels (map #(as-sel (.item sels %)) (range (.getLength sels)))]
      (fn [loc]
        (mapcat #(% loc) sels))))
  org.w3c.css.sac.ElementSelector
  (as-sel [sel]
    (fn [loc]
      (when (some-> (loc/node loc) :tag name (tag-is? (.getLocalName sel)))
        (list loc))))
  org.w3c.css.sac.DescendantSelector
  (as-sel [sel]
    (sel/chain (as-sel (.getAncestorSelector sel))
      (case (.getSelectorType sel)
        #=(eval org.w3c.css.sac.Selector/SAC_CHILD_SELECTOR)
        children
        #=(eval org.w3c.css.sac.Selector/SAC_DESCENDANT_SELECTOR)
        descendants)
      (as-sel (.getSimpleSelector sel))))
  org.w3c.css.sac.ConditionalSelector
  (as-sel [sel]
    (sel/chain (as-sel (.getSimpleSelector sel)) (as-sel (.getCondition sel))))
  org.w3c.css.sac.AttributeCondition
  (as-sel [cond]
    (let [pred (case (long (.getConditionType cond))
                 #=(eval org.w3c.css.sac.Condition/SAC_ATTRIBUTE_CONDITION)
                 (let [k (keyword (.getLocalName cond))]
                   (if (.getSpecified cond)
                     (let [v (.getValue cond)]
                       #(some-> % loc/node :attrs (get k) (= v)))
                     #(some-> % loc/node :attrs (get k))))
                 #=(eval org.w3c.css.sac.Condition/SAC_CLASS_CONDITION)
                 #(when-let [classes (some->> % loc/node :attrs :class (re-seq #"\S+") set)]
                    (contains? classes (.getValue cond)))
                 #_#_#=(eval org.w3c.css.sac.Condition/SAC_PSEUDO_CLASS_CONDITION)
                   (case (.getValue cond)
                     "first-of-type")
                 #=(eval org.w3c.css.sac.Condition/SAC_ID_CONDITION)
                 #(some-> % loc/node :attrs :id (= (.getValue cond))))]
      (fn [loc] (when (pred loc) (list loc)))))
  #_org.w3c.css.sac.CombinatorCondition
  #_org.w3c.css.sac.NegativeCondition)


(defn at [& selector+transformations]
  (grounder/at* selector+transformations sel))

;; html-specific transformations
(defn class
  "Set a class (on the selected elements) when the value at the corresponding path in the model is true."
  {:arglists '([class path & class+paths])}
  [& class+paths]
  (grounder/composite-transformation
    (for [[class path] (partition 2 class+paths)]
      (grounder/simple-transformation
        (sel/chain element (sel/by-path [:attrs :class classes (name class)]))
        (action/replace path)))))

(defn style
  "Sets a style (on the selected elements) to the value at the corresponding path in the model."
  {:arglists '([style path & style+paths])}
  [& style+paths]
  (grounder/composite-transformation
    (for [[style path] (partition 2 style+paths)]
      (grounder/simple-transformation
        (sel/chain element (sel/by-path [:attrs :style styles (name style)]))
        (action/replace path)))))


(defn attr
  "Set an attribute (on the selected elements) to the value at the corresponding path in the model."
  {:arglists '([attr path & attr+paths])}
  [& attr+paths]
  (grounder/composite-transformation
    (for [[attr path] (partition 2 attr+paths)]
      (grounder/simple-transformation
        (sel/chain element (sel/by-path [:attrs (keyword attr)]))
        (action/replace path)))))

(defn content
  "Set the content (of the selected elements) the value at the path in the model."
  [path]
  (grounder/simple-transformation
    (sel/chain element (sel/by-path [:content (seg/slice 0 java.lang.Long/MAX_VALUE)]))
    (action/replace path)))

(defn prepend [path]
  (grounder/simple-transformation
    (sel/chain element (sel/by-path [:content (seg/slice 0 0)]))
    (action/replace path)))

(defn append [path]
  (grounder/simple-transformation
    (sel/chain element (sel/by-path [:content (seg/slice java.lang.Long/MAX_VALUE java.lang.Long/MAX_VALUE)]))
    (action/replace path)))

(defn dup
  "Dup[licate] the selected nodes for each item in the collection at the path in the model.
  Each copy is then transformed using the specified transformations.
  For these transformatons the model is restricted to the item."
  [path & transformations]
  (grounder/simple-transformation
    list
    (action/dup path (apply at transformations))))

#_(defn if [selector test then else]
   [[selector [::action/if 0 [test] then else]]])

#_(defn discard [selector]
   [[selector action/discard]])

(defn static-template [node & transformations]
  (let [plan (plan/plan (grounder/ground (apply at transformations) node))
        emitted (common/tight-fn-emit! (static/prerender node plan common/tight-fn-emit! (common/tight-fn-emit!)))]
    (fn
      ([data] (common/render emitted data))
      ([data emit acc] (common/render emitted data emit acc)))))





     
