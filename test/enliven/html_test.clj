(ns enliven.html-test
  (:use clojure.test)
  (:require 
   [enliven.html :as h :refer [static-template content class
                               append prepend style]]
   [enliven.html.jsoup :as jsoup]))


(def node (jsoup/parse "<div class='fun'>"))
(def node2 (jsoup/parse "<div><span>t</span></div>"))
(def node3 (jsoup/parse "<div style='content: url(welcome), \"testing; \" url(logo);'>"))


(defn page-wrap [node-str]
  (str "<html><head></head><body>" node-str "</body></html>"))

(deftest transform-tests
  (testing "content transform"
    (let [trans (static-template node :div (content :content))]
      (is (= (page-wrap "<div class='fun'>test</div>")
             (trans {:content "test"})))))
  (testing "class transform"
    (let [trans (static-template node :div (class :test :success))]
      (is (= (page-wrap "<div class='test fun'></div>")
             (trans {:success true})))))
  (testing "append transform"
    (let [trans (static-template node2 :div (append :success))]
      (is (= (page-wrap "<div><span>t</span>test</div>")
             (trans {:success "test"})))))
   (testing "prepend transform"
    (let [trans (static-template node2 :div (prepend :success))]
      (is (= (page-wrap "<div>test<span>t</span></div>")
             (trans {:success "test"})))))
   (testing "style transform - simple case"
     (let [trans (static-template node2 :div (style :display :display
                                                    :font-size :fs))]
      (is (= (page-wrap "<div style='display:none;font-size:12px;'><span>t</span></div>")
             (trans {:display "none"
                     :fs "12px"})))))
   (testing "style transform - with embedded strings"
     (let [trans (static-template node3 :div (style :color :color))]
      (is (= (page-wrap "<div style='color:red;content:url(welcome), \"testing; \" url(logo);'></div>")
             (trans {:color "red"}))))))

