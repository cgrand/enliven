(ns enliven.core.paths-test
  (:use clojure.test)
  (:require
    [enliven.html.model :as html]
    [enliven.text.model :as text]
    [enliven.core.lenses :as lens]))

(deftest canonicalization
  (testing "Hoisting segments"
    (is (= (lens/canonical :foo) :foo))
    (is (= (lens/canonical [:foo]) :foo)))
  (testing "empty path is the identity"
    (is (= (lens/canonical []) lens/identity)))
  (testing "Merge nested slices"
    (is (= (lens/canonical [(lens/slice 1 3) (lens/slice 1 2)])
          (lens/slice 2 3))))
  (testing "Reparent indexed segment as a 0-index in a singleton slice."
    (is (= (lens/decompose (lens/canonical [12]))
          [(lens/slice 12 13) 0]))
    (is (= (lens/decompose (lens/canonical [(lens/slice 2 20) 12]))
             [(lens/slice 14 15) 0])))
  (testing "Simplify constant paths"
    (is (= (lens/canonical [:foo (lens/const 42)])
          (lens/const 42)))
    (is (= (lens/canonical [:foo (lens/const 42) (lens/const 63)])
          (lens/const 63)))
    (is (= (lens/canonical [:foo (lens/const {:pi 3.14}) :pi])
          (lens/const 3.14)))
    (is (= (lens/canonical [:foo (lens/const {:pi 3.14}) :pi (lens/const 42)])
          (lens/const 42)))))

(deftest abstract-fetch
  (are [from path to] (= (lens/fetch-type from path) to)
    ::html/node [:content (lens/slice 1 2) 0 :attrs :class html/classes "important"] ::lens/boolsy
    ::html/node [:content (lens/slice 1 2) 0 :attrs :style text/chars] ::text/chars))
