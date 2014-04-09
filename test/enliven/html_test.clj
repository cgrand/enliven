(ns enliven.html-test
  (:use clojure.test)
  (:refer-clojure :exclude [class])
  (:require
   [enliven.html :as h :refer [static-template content class
                               append prepend style]]
   [enliven.html.jsoup :as jsoup]
   [enliven.core.locs :as loc]
   [enliven.core.lenses :as lens]))


(deftest selector-tests
  (are [sel e] (not-empty ((h/css sel) (loc/loc e)))
    "div" {:tag :div}
    "[id]" {:tag :div :attrs {:id "foo"}}
    "[id=foo]" {:tag :div :attrs {:id "foo"}}
    "[class~=foo]" {:tag :div :attrs {:class "foo bar baz"}}
    "[class~=bar]" {:tag :div :attrs {:class "foo bar baz"}}
    "[class~=baz]" {:tag :div :attrs {:class "foo bar baz"}}
    "[lang|=fr]" {:tag :div :attrs {:lang "fr"}}
    "[lang|=fr]" {:tag :div :attrs {:lang "fr-be"}}
    "[class^=fo]" {:tag :div :attrs {:class "foo bar baz"}}
    "[class*=ba]" {:tag :div :attrs {:class "foo bar baz"}}
    "[class$=az]" {:tag :div :attrs {:class "foo bar baz"}}
    "div#id.foo" {:tag :div :attrs {:class "foo bar baz" :id "id"}}
    "div #id.foo" {:tag :div :content [{:tag :span :attrs {:class "foo bar baz" :id "id"}}]})
  (are [sel e] (empty? ((h/css sel) (loc/loc e)))
    "span" {:tag :div}
    "[id]" {:tag :div :attrs {:class "foo"}}
    "[id=bar]" {:tag :div :attrs {:id "foo"}}
    "[class~=fizz]" {:tag :div :attrs {:class "foo bar baz"}}
    "[lang|=fr]" {:tag :div :attrs {:lang "de"}}
    "[lang|=fr]" {:tag :div :attrs {:lang "be-fr"}}
    "[class^=oo]" {:tag :div :attrs {:class "foo bar baz"}}
    "[class*=boom]" {:tag :div :attrs {:class "foo bar baz"}}
    "[class$=ba]" {:tag :div :attrs {:class "foo bar baz"}}
    "span#id.foo" {:tag :div :attrs {:class "foo bar baz" :id "id"}}))

(def node (jsoup/parse "<div>"))
(def node2 (jsoup/parse "<div><span>t</span></div>"))
(def node3 (jsoup/parse "<ul><li><span class=a></span><span class=b></span>"))
(def node4 (jsoup/parse "<div style='background-color:green;color:blue;'>"))

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
  (testing "style tranform - insert style"
    (let [trans (static-template node :div (style :color :color))]
      (is (= (page-wrap "<div style='color:red;'></div>")
             (trans {:color "red"})))))
  (testing "style tranform - update style"
    (let [trans (static-template node4 :div (style :background-color
                                                   :color))]
      (is (= (page-wrap "<div style='color:blue;background-color:red;'></div>")
             (trans {:color "red"})))))
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

(deftest const-analysis-tests
  (let [t (static-template node3
            :li (h/dup (lens/const ["Hallo" ["Bonjour"]])
                  :span.a (content []))
            :span.b (content :msg))]
    (is (= (page-wrap "<ul><li><span class='a'>Hallo</span><span class='b'>x</span></li><li><span class='a'>Bonjour</span><span class='b'>x</span></li></ul>")
          (t {:msg "x"})))
    (is (= ["x"] (distinct
                    (keep (fn [[x y]] (when-not (identical? x y) x))
                      (map vector (t {:msg "x"} conj []) (t {:msg "y"} conj []))))))))

(deftest idempotency-tests
  (let [t (static-template node3
            :li (h/dup []
                  :span.a (content []))
            :li (h/dup []
                  :span.a (content [])))]
    (is (= (page-wrap "<ul><li><span class='a'>1</span><span class='b'></span></li><li><span class='a'>2</span><span class='b'></span></li></ul>")
          (t (map str (range 1 3))))))
  (let [t (static-template node3
            :li (h/dup []
                  :span (content []))
            :li (h/dup []
                  :span.a (content [])))]
    (is (= (page-wrap "<ul><li><span class='a'>1</span><span class='b'>1</span></li><li><span class='a'>2</span><span class='b'>2</span></li></ul>")
          (t (map str (range 1 3))))))
  (let [t (static-template node3
            :li (h/dup []
                  :span.b (content []))
            :li (h/dup []
                  :span.a (content [])))]
    (is (= (page-wrap "<ul><li><span class='a'>1</span><span class='b'>1</span></li><li><span class='a'>2</span><span class='b'>2</span></li></ul>")
          (t (map str (range 1 3)))))))

(deftest escaping-test
  (let [t (static-template node3
            :span.a (content (lens/const "<&\"'"))
            :span.a (h/attr :class (lens/const "<&\"'"))
            :span.b (content [])
            :span.b (h/attr :class []))]
    (is (= (page-wrap
             (let [s "<span class='<&\"&quot;'>&lt;&amp;\"'</span>"]
               (str "<ul><li>" s s "</li></ul>")))
          (t "<&\"'")))))
