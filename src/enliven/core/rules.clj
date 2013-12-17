(ns enliven.core.rules
  (:require [enliven.core.paths :as paths]
    [enliven.core.actions :as action]))

;; BIG PICTURE
;; 1/ selectors and transformations on data-paths
;; 2/ run selectors on actual html and get node-paths
;; 3/ mash them together

;; an op is [type depth data-paths & more] where type is a namespaced keyword
;; the addutional args depend on the type of the op.

;; a rule is [node-path op]
;; a transformation is a coll of rules

;; The data context passed at runtime to the op is a list of context, that's why the
;; depth is useful

(def id "The identity transformation" #{})

;; A transformation is a set of rules, by construction the following properties are enforced:
;; * there are no two rules whose paths are prefix of one another
;; * all paths are canonical

(defn- rule-type [[_ [type]]]
  type)

(defmulti ^:private mash (fn [prefix-rule other-rule]
                           (rule-type prefix-rule)))

(defmethod mash :default [prefix-rule other-rule]
  (if (= prefix-rule other-rule)
    prefix-rule
    (throw (ex-info "Conflicting rules" {:rules [prefix-rule other-rule]}))))

(defn conj-rule [rules [path action]]
  ; there is only three way to interact with existing rules:
  ; * either the path of the new rule is prefix of several existing rules
  ; * either the path of the new rule is prefixed by one existing rule
  ; (* the paths are conflicting)
  (let [path (paths/canonical path)
        rule [path action]]
    (if-let [prefix (some #(when (paths/broad-prefix? (first %) path) %) rules)]
      (-> rules (disj prefix) (conj (mash prefix rule)))
      (let [prefixees (filter #(paths/broad-prefix? path (first %)) rules)]
        (-> (reduce disj rules prefixees)
          (conj (reduce mash rule prefixees)))))))

(declare ^:private nest)

(defn- rebase-rule [[path action] prefix]
  [(paths/remove-prefix prefix path) action])

(defmethod mash ::action/dup [[path [op n args subrules]] other-rule]
  [path [op n args (conj-rule subrules (-> other-rule (rebase-rule path) nest))]])

(defn- nest-rules [rules]
  (set (map nest rules)))

(defn- nest [[path action]]
  [path (-> action (assoc 1 (inc (nth action 1)))
          (action/update-subs nest-rules))])

(defmethod mash ::action/discard
  [discard-rule _]
  discard-rule)