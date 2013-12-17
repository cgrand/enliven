(ns enliven.html.css-selectors
  (:require [enliven.core.selectors :as sel]
    [enliven.core.locs :as loc]))

(defn children [loc]
  (when (:tag (loc/node loc))
    (let [loc (loc/down loc :content)]
      (for [i (range (count (loc/node loc)))]
        (loc/down loc i)))))

(defn descendants [loc]
  (mapcat #(cons % (descendants %)) (children loc)))

(defn descendants-and-self [loc]
  (cons loc (descendants loc)))

(defn loose-chain 
  ([] list)
  ([& sels] (reduce sel/chain (interpose descendants sels))))

(defprotocol ^:private ToSelector
  (as-sel [x]))

(defn css 
  ([s]
    (css s false))
  ([^String s rooted]
    (let [sel (-> (org.apache.batik.css.parser.Parser.)
                (.parseSelectors s)
                as-sel)]
      (if rooted
        sel
        (sel/chain descendants-and-self sel)))))

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
