(ns enliven.core.paths-test
  (:use clojure.test)
  (:require
    [enliven.html :as html]
    [enliven.text :as text]
    [enliven.core.paths :as path]
    [enliven.core.segments :as seg]))

(deftest canonicalization
  (testing "Hoisting segments"
    (is (= (path/canonical :foo) [:foo]))
    (is (= (path/canonical [:foo]) [:foo])))
  (testing "nil is an empty path"
    (is (= (path/canonical nil) [])))
  (testing "Merge nested slices"
    (is (= (path/canonical [(seg/slice 1 3) (seg/slice 1 2)])
          [(seg/slice 2 3)])))
  (testing "Reparent indexed segment as a 0-index in a singleton slice."
    (is (= (path/canonical [12])
          [(seg/slice 12 13) 0]))
    (is (= (path/canonical [(seg/slice 2 20) 12])
             [(seg/slice 14 15) 0])))
  (testing "Simplify constant paths"
    (is (= (path/canonical [:foo (seg/const 42)])
          [(seg/const 42)]))
    (is (= (path/canonical [:foo (seg/const 42) (seg/const 63)])
          [(seg/const 63)]))
    (is (= (path/canonical [:foo (seg/const {:pi 3.14}) :pi])
          [(seg/const 3.14)]))
    (is (= (path/canonical [:foo (seg/const {:pi 3.14}) :pi (seg/const 42)])
          [(seg/const 42)]))))

(deftest abstract-fetch
  (are [from path to] (= (path/fetch-type-in from path) to)
    ::html/node [:content (seg/slice 1 2) 0 :attrs :class html/classes "important"] ::seg/boolsy
    ::html/node [:content (seg/slice 1 2) 0 :attrs :style text/chars] ::text/chars))