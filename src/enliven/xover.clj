(ns enliven.xover
  (:refer-clojure :exclude [def defn ns])
  (:require 
    [clojure.core :as core]
    [cljs.closure :as cljsc]))

;; cross-compilation stuff
(defn- emit-cljs-ns [ns]
  ;; TODO: cover macros in other namespaces
  (let [macros (keep (fn [[k v]] (when (-> v meta :macro) k))
                 (ns-interns *ns*))]
    (list 'ns (ns-name ns)
      (list :use-macros [(ns-name ns) :only macros]))))

(defn- store-cljs-def 
  "Stores a form for eventual cljs compilation. Returns the stored form."
  [form ns name]
  (alter-meta! ns assoc-in [::cljs-def-cache name] form)
  form)

; def being a special form you can't shadow it (as of 1.5.1)
(def ^:dynamic ^:private *target* :clj)

;; public fns and macros
(core/defn build [opts ns]
  (let [forms (-> ns meta ::cljs-def-cache vals)
        js (binding [*target* :cljs]
             (cljsc/compile-form-seq
               (cons (emit-cljs-ns ns) forms)))
        srcs (cljsc/add-dependencies {} js)]
    (apply cljsc/optimize opts srcs)))

(defmacro expr [& env-exprs]
  ((apply hash-map env-exprs) *target*))

(defmacro defn [name & more]
  (store-cljs-def `(core/defn ~name ~@more) *ns* name))

(defmacro def [name & more]
  ;; def being a special form needs to be handled differently than defn
  (store-cljs-def `(def ~name ~@more) *ns* name))

;; TODO: ns
