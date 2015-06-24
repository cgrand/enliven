(ns enliven.core.plans_test
  (:use clojure.test)
  (:require [enliven.core.plans :as plan]
    [enliven.core.lenses :as lens]))

(deftest some-sets
  (is
    (= (plan/exec {:title "Hey look I'm the source!"}
          (plan/plan
            [{:src-lens :title
              :data-lens :a-key
              :rule-type :set}])
          {:a-key "Hey look I changed!"})
      {:title "Hey look I changed!"}))
  (is
    (= (plan/exec {:title "Hey look I'm the source!"
                   :asbtract "TBD"}
          (plan/plan
            [{:src-lens :asbtract
              :data-lens :a-key
              :rule-type :set}])
          {:a-key "Hey look I changed!"})
      {:title "Hey look I'm the source!"
       :asbtract "Hey look I changed!"}))
  (is
    (= (plan/exec {:title "Hey look I'm the source!"
                   :items ["one" "two" "three"]}
          (plan/plan
            [{:src-lens [:items 1]
              :data-lens :a-key
              :rule-type :set}])
          {:a-key "Hey look I changed!"})
      {:title "Hey look I'm the source!"
       :items ["one" "Hey look I changed!" "three"]})))

(deftest some-dups
  (is
    (= (plan/exec {:header {:title 42
                            :author "Me Myself I"}
                   :items [{:title "sample item"}]}
         (plan/plan [{:src-lens [:header :title]
                      :data-lens :a
                      :rule-type :set}
                     {:src-lens [:items (lens/slice 0 1) 0 :copyright]
                      :data-lens :c
                      :rule-type :set}
                     {:src-lens [:items (lens/slice 0 1)]
                      :data-lens :b
                      :rule-type :dup
                      :subrules [{:src-lens [0 :title]
                                  :data-lens []
                                  :rule-type :set}]}])
         {:a 2 :b ["Real Item #1" "Real Item #2"] :c "cgrand"})
      {:header {:title 2, :author "Me Myself I"}, 
       :items [{:title "Real Item #1", :copyright "cgrand"}
               {:title "Real Item #2", :copyright "cgrand"}]})))
