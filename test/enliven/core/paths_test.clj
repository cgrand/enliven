(ns enliven.core.paths-test
  (:use clojure.test)
  (:require
    [enliven.html.model :as html]
    [enliven.text.model :as text]
    [enliven.core.lenses :as lens]))

(deftest simplification
  (testing "Hoisting segments"
    (is (= (lens/simplify :foo) :foo))
    (is (= (lens/simplify [:foo]) :foo)))
  (testing "empty path is the identity"
    (is (= (lens/simplify []) lens/identity)))
  (testing "Merge nested slices"
    (is (= (lens/simplify [(lens/slice 1 3) (lens/slice 1 2)])
          (lens/slice 2 3))))
  (testing "Reparent indexed segment as a 0-index in a singleton slice."
    (is (= (lens/simplify [12])
          12))
    (is (= (lens/simplify [(lens/slice 2 20) 12])
             14)))
  (testing "Simplify constant paths"
    (is (= (lens/simplify [:foo (lens/const 42)])
          (lens/const 42)))
    (is (= (lens/simplify [:foo (lens/const 42) (lens/const 63)])
          (lens/const 63)))
    (is (= (lens/simplify [:foo (lens/const {:pi 3.14}) :pi])
          (lens/const 3.14)))
    (is (= (lens/simplify [:foo (lens/const {:pi 3.14}) :pi (lens/const 42)])
          (lens/const 42)))))

(deftest gcl
  (is (= (lens/gcl 1 1)
        [1 lens/identity lens/identity]))
  (is (= (lens/gcl 1 2)
        [lens/identity 1 2]))
  (is (= (lens/gcl (lens/slice 2 6) (lens/slice 4 7))
        nil))
  (is (= (lens/gcl 3 (lens/slice 2 5))
        [(lens/slice 2 5) 1 lens/identity]))
  (is (= (lens/gcl (lens/slice 3 4) (lens/slice 2 5))
        [(lens/slice 2 5) (lens/slice 1 2) lens/identity])))

(deftest abstract-fetch
  (are [from path to] (= (lens/fetch-type from path) to)
    ::html/node [:content (lens/slice 1 2) 0 :attrs :class html/classes "important"] ::lens/boolsy
    ::html/node [:content (lens/slice 1 2) 0 :attrs :style text/chars] ::text/chars))
