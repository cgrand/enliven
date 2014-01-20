(ns enliven.html-test
  (:use clojure.test)
  (:require 
   [enliven.html :as h :refer [static-template content class
                               append prepend]]
   [enliven.html.jsoup :as jsoup]))


(def node (jsoup/parse "<div>"))
(def node2 (jsoup/parse "<div><span>t</span></div>"))

(defn page-wrap [node-str]
  (str "<html><head></head><body>" node-str "</body></html>"))

(deftest transform-tests
  (testing "content tranform"
    (let [trans (static-template node :div (content :content))]
      (is (= (page-wrap "<div>test</div>")
             (trans {:content "test"})))))
  (testing "class tranform"
    (let [trans (static-template node :div (class :test :success))]
      (is (= (page-wrap "<div class='test'></div>")
             (trans {:success true})))))
  (testing "append tranform"
    (let [trans (static-template node2 :div (append :success))]
      (is (= (page-wrap "<div><span>t</span>test</div>")
             (trans {:success "test"})))))
   (testing "prepend tranform"
    (let [trans (static-template node2 :div (prepend :success))]
      (is (= (page-wrap "<div>test<span>t</span></div>")
             (trans {:success "test"})))))
   (testing "dup tranform"
    (let [trans (static-template node :div (h/dup :items (content [])))]
      (is (= (page-wrap "<div>0</div><div>1</div><div>2</div>")
             (trans {:items (map str (range 3))}))))))

