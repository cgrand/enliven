(ns enliven.core-test
  (:use clojure.test)
  (:require 
    [enliven.core :as c]
    [enliven.html :as h]))

(deftest path-order
  (testing "path comparator"
    (is (= (c/order-by-path identity [[9] [6] [8] [4] [3] [5] [2] [1] [0] [7]])
          [[9] [8] [7] [6] [5] [4] [3] [2] [1] [0]]))
    (is (= (c/order-by-path identity [[9] [6] [8] [[4 6]] [3] [2] [1] [0] [7]])
          [[9] [8] [7] [6] [[4 6]] [3] [2] [1] [0]]))
    (is (= (c/order-by-path identity [[9] [6] [8] [[4 6]] [3] [[0 3]] [7]])
          [[9] [8] [7] [6] [[4 6]] [3] [[0 3]]]))
    (is (= (c/order-by-path identity [[0 9] [0 6] [0 8] [0 [4 6]] [0 3] [1 [0 3]] [7]])
          [[7] [1 [0 3]] [0 9] [0 8] [0 6] [0 [4 6]] [0 3]]))))

(defn eq-to [node html-string]
  (= (-> node h/render h/parse) (h/parse html-string)))

(deftest first-duplicate
  (is (eq-to (c/transform (h/duplicate "li" [:items]
                            (c/compose
                              (h/content)
                              (h/attr c/here :class (c/const "zoo"))))
               (h/parse "<ul><li><li>")
               {:items [1 2 3]})
        "<ul><li class=zoo>1<li class=zoo>2<li class=zoo>3")))