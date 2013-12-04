(ns enliven.html
  (:require 
    [enliven.core :as c]
    #_[enliven.xover :as x]
    [enliven.html.jsoup :as jsoup]
    [clojure.string :as str]))

(defn children [loc]
  (when (:tag (c/node loc))
    (let [loc (c/down loc :content)]
      (for [i (range (count (c/node loc)))]
        (c/down loc i)))))

(defn descendants [loc]
  (mapcat #(cons % (descendants %)) (children loc)))

(defn descendants-and-self [loc]
  (cons loc (descendants loc)))

(defn loose-chain [& sels]
  (reduce c/chain c/here (interpose descendants sels)))

(defn css 
  ([s]
    (css s false))
  ([^String s rooted]
    (let [sel (-> (org.apache.batik.css.parser.Parser.)
                (.parseSelectors s))]
      (if rooted
        sel
        (c/chain descendants-and-self sel)))))

(defn- tag-is? [tag sel]
  (or (nil? sel) (= tag sel)))

(extend-protocol c/Selector
  String
  (-select [s loc]
    (c/-select (css s) loc))
  clojure.lang.Keyword
  (-select [k loc]
    (c/-select (css (name k)) loc))
  org.w3c.css.sac.SelectorList
  (-select [sels loc]
    (mapcat #(c/-select (.item sels %) loc) (range (.getLength sels))))
  org.w3c.css.sac.ElementSelector
  (-select [sel loc]
    (when (some-> (c/node loc) :tag name (tag-is? (.getLocalName sel)))
      (c/here loc)))
  org.w3c.css.sac.DescendantSelector
  (-select [sel loc]
    (let [f (case (.getSelectorType sel)
              #=(eval org.w3c.css.sac.Selector/SAC_CHILD_SELECTOR)
              children
              #=(eval org.w3c.css.sac.Selector/SAC_DESCENDANT_SELECTOR)
              descendants)]
      (c/-select (c/chain (.getAncestorSelector sel) f (.getSimpleSelector sel))
        loc)))
  org.w3c.css.sac.ConditionalSelector
  (-select [sel loc]
    (c/-select (c/chain (.getSimpleSelector sel) (.getCondition sel))
      loc))
  org.w3c.css.sac.AttributeCondition
  (-select [cond loc]
    (when (case (long (.getConditionType cond))
            #=(eval org.w3c.css.sac.Condition/SAC_ATTRIBUTE_CONDITION)
            (if (.getSpecified cond) 
              (some-> loc c/node :attrs 
                (get (keyword (.getLocalName cond)))
                (= (.getValue cond)))
              (some-> loc c/node :attrs 
                (get (keyword (.getLocalName cond)))))
            #=(eval org.w3c.css.sac.Condition/SAC_CLASS_CONDITION)
            (when-let [s (some->> loc c/node :attrs :class (re-seq #"\S+") set)]
              (s (.getValue cond)))
            #_#_#=(eval org.w3c.css.sac.Condition/SAC_PSEUDO_CLASS_CONDITION)
            (case (.getValue cond)
              "first-of-type")
            #=(eval org.w3c.css.sac.Condition/SAC_ID_CONDITION)
            (some-> loc c/node :attrs :id (= (.getValue cond))))
      (c/here loc)))
  #_org.w3c.css.sac.CombinatorCondition
  #_org.w3c.css.sac.NegativeCondition)

(def classes
  (c/path-segment 
    :get #(set (when % (re-seq #"\S+" %)))
    :put #(str/join " " %)))

#_(defn tag= [tag]
   (fn [loc]
     (when (= (:tag (c/node loc)) tag)
       (c/here loc))))

(defn -content [_ x] [(str x)])
(defn -attr-value [_ x] (str x))

(defn- slicer [slices]
  (fn [loc]
    (let [node (c/node loc)]
      (when (vector? node)
        (map #(c/down loc %) (slices node))))))

(defn content
  ([] (content []))
  ([data-path] (content c/here data-path))
  ([selector data-path]
    [[(c/chain selector 
        (c/path-selector [:content])
        (slicer #(list [0 (count %)])))
      -content (c/data-arg data-path)]]))

(defn prepend 
  ([] (prepend []))
  ([data-path] (prepend c/here data-path))
  ([selector data-path]
    [[(c/chain selector (c/path-selector [:content [-1 0]])) -content (c/data-arg data-path)]]))

(defn append 
  ([] (append []))
  ([data-path] (append c/here data-path))
  ([selector data-path]
    [[(c/chain selector 
        (c/path-selector [:content])
        (slicer #(let [n (count %)] [[n (inc n)]])))
      -content (c/data-arg data-path)]]))

(defn attr [selector? & attr+paths]
  (let [[selector kps] (if (odd? (count attr+paths))
                         [c/here (cons selector? attr+paths)]
                         [selector? attr+paths])]
    (for [[k p] (partition 2 kps)] 
      [(c/chain selector (c/path-selector [:attrs k])) -attr-value (c/data-arg p)])))

(defn -class [_ x] x)

(defn class [selector? & class+paths]
  (let [[selector kps] (if (odd? (count class+paths))
                         [c/here (cons selector? class+paths)]
                         [selector? class+paths])]
    (for [[k p] (partition 2 kps)] 
      [(c/chain selector (c/path-selector [:attrs :class classes k])) -class (c/data-arg p)])))

(defn whitespace? [node]
  (and (string? node) (= (str/trim node) "")))

(defn comment? [node]
  (contains? node :comment))

(defn- rangeify [x]
  (if (vector? x)
    x
    [x (inc x)]))

(defn- contiguous? [loc-left loc-right]
  (let [pleft (c/path loc-left)
        pright (c/path loc-right)]
    (and (= (pop pleft) (pop pright))
      (let [[_ from] (rangeify (peek pleft))
            [to] (rangeify (peek pright))
            all-siblings (-> loc-left c/up c/node)]
        (every? #(let [node (nth all-siblings %)]
                   (or (whitespace? node) (comment? node)))
          (range from to))))))

(defn- first-siblings-only [selector]
  (fn [loc]
    (when-let [[leftmost-loc :as locs] (seq (c/order-by-path true c/path 
                                              (c/select loc selector)))]
      (cons leftmost-loc
        (keep (fn [[left right]]
                (when-not (contiguous? left right)
                  right))
          (partition 2 1 locs))))))

(defn- next-siblings-only [selector]
  (fn [loc]
    (let [[leftmost-loc :as locs] (c/order-by-path true c/path 
                                    (c/select loc selector))]
      (keep (fn [[left right]]
              (when (contiguous? left right)
                right))
        (partition 2 1 locs)))))

;; TODO: introduce special notation for "detached" nodes
;; removal is a case of being detached into oblivion

(defn duplicate [selector path t]
  [[(next-siblings-only selector)
    (constantly (c/splice [])) [:sub [] nil]]
   [(first-siblings-only selector)
    (fn [node f coll]
      (c/splice (reduce (fn [v x]
                          (let [x (f x)]
                            (if (sequential? x)
                              (into v x)
                              (conj v x)))) 
                 [] coll))) [:sub [] t] (c/data-arg path)]])

;; emitting
(defn- escape-attr
 "Escapes < > & and \"."
 [^String x]
  (-> x (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;") (.replace "\"" "&quot;")))

(declare render)

(defmacro unroll-> 
  [x f & forms] 
  `(-> ~x ~@(map #(list f %) forms)))

(defn render-element [{:keys [tag content attrs]} emit out]
  (-> out
    (unroll-> emit "<" (name tag))
    (as-> out
      (reduce-kv (fn [out attr val] 
                   (unroll-> out emit " " (name attr) 
                     "='" (escape-attr val) "'"))
        out attrs))
    (emit ">")
    (as-> out (reduce #(render %2 emit %1) out content))
    (unroll-> emit "</" (name tag) ">")))

(defn render-comment [{:keys [comment]} emit out]
  ; TODO: escape?
  (emit out "<!--" comment "-->"))



(defn render
  ([node]
    (str (render node #(.append ^StringBuilder %1 %2) (StringBuilder.))))
  ([node emit out]
    (cond
      (:tag node) (render-element node emit out)
      (:comment node) (render-comment node emit out)
      (string? node) (emit out node)
      :else (throw (ex-info "Cannot render" node)))))

(defn parse [^String s]
  (first (jsoup/->nodes (org.jsoup.Jsoup/parse s))))

;; 
(defn template [src & ts]
  (c/bind
    (apply c/compose ts)
    (parse src)))