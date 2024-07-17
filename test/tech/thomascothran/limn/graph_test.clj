(ns tech.thomascothran.limn.graph-test
  (:require [clojure.test :refer [deftest testing is]]
            [tech.thomascothran.limn.graph :as g]))

(deftest test-associate-actions-with-deps
  (let [workflow
        {:workflow/actions
         {:root {:action/requires #{}
                 :action/produces #{:a}}
          :middle {:action/requires #{:a}
                   :action/produces #{:b}}
          :leaf {:action/requires #{:b}
                 :action/produces #{:c}}
          :island {:action/requires #{:y}
                   :action/produces #{:z}}}}]
    (testing "producers"

      (is (= #{:root}
             (-> (g/associate-action-with-deps
                  workflow :root {})
                 (get-in [:a :product/producers])))
          "Should merge the product")

      (is (= #{:root :pre-existing}
             (->> {:a {:product/producers #{:pre-existing}}}
                  (g/associate-action-with-deps
                   workflow :root)
                  (#(get-in % [:a :product/producers]))))
          "Should merge the product")

      (is (nil? (-> (g/associate-action-with-deps
                     workflow :root {})
                    (get-in [:a :product/requirers])))
          "Should only add to the requirers"))

    (testing "requirers"
      (is (= #{:middle}
             (-> (g/associate-action-with-deps
                  workflow
                  :middle
                  {})
                 (get-in [:a :product/requirers]))))

      (is (= #{:pre-existing :middle}
             (->> {:a {:product/requirers #{:pre-existing}}}
                  (g/associate-action-with-deps
                   workflow
                   :middle)
                  (#(get-in % [:a :product/requirers]))))))))

(deftest test-products->actions
  (let [workflow
        {:workflow/actions
         {:root {:action/requires #{}
                 :action/produces #{:a}}
          :middle {:action/requires #{:a}
                   :action/produces #{:b}}
          :leaf {:action/requires #{:b}
                 :action/produces #{:c}}
          :island {:action/requires #{:y}
                   :action/produces #{:z}}}}
        expected
        {:a {:product/producers #{:root}, :product/requirers #{:middle}},
         :b {:product/producers #{:middle}, :product/requirers #{:leaf}},
         :c {:product/producers #{:leaf}},
         :y {:product/requirers #{:island}},
         :z {:product/producers #{:island}}}

        result (g/products->actions workflow)]

    (is (= expected result))))

(deftest test-dependencies-graph
  (let [workflow
        {:workflow/actions
         {:root {:action/requires #{}
                 :action/produces #{:a}}
          :middle {:action/requires #{:a}
                   :action/produces #{:b}}
          :leaf {:action/requires #{:b}
                 :action/produces #{:c}}
          :island {:action/requires #{:y}
                   :action/produces #{:z}}}}
        expected-graph
        {:middle #{:root}
         :leaf #{:middle}}

        actual-graph (g/action-graph workflow)]

    (is (= expected-graph actual-graph))))

