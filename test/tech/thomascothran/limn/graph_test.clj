(ns tech.thomascothran.limn.graph-test
  (:require [clojure.test :refer [deftest testing is]]
            [tech.thomascothran.limn.adapters]
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

        actual-graph (g/children->parents workflow)]

    (is (= expected-graph actual-graph))))

(def root->leaf-g
  {:workflow/actions
   {:root {:action/requires #{}
           :action/produces #{:a}}
    :middle-a {:action/requires #{:a}
               :action/produces #{:b}}
    :middle-b {:action/requires #{:a}
               :action/produces #{:b}}
    :leaf {:action/requires #{:b}
           :action/produces #{:c}}
    :island {:action/requires #{:y}
             :action/produces #{:z}}}})

(deftest test-all-paths-diamond
  (is (= #{[:root :middle-b :leaf] [:root :middle-a :leaf]}
         (g/all-paths root->leaf-g :root :leaf))))

(def reachable-cycle
  {:workflow/actions
   {:root {:action/requires #{}
           :action/produces #{:a}}
    :on-a {:action/requires #{:a}
           :action/produces #{:b}}
    :on-b {:action/requires #{:b}
           :action/produces #{:a :c}}
    :on-c {:action/requires #{:c}
           :action/produces #{:done}}}})

(deftest test-with-cycle
  (is (= #{[:root :on-a :on-b :on-c]}
         (g/all-paths reachable-cycle :root :on-c))))

(def cyclic-dep-block
  {:workflow/actions
   {:root {:action/requires #{}
           :action/produces #{:a}}
    :on-a {:action/requires #{:a :b}
           :action/produces #{:spin :c}}
    :on-spin {:action/requires #{:spin}
              :action/produces #{:b}}
    :on-c {:action/requires #{:c}
           :action/produces #{:done}}}})

(deftest test-cyclic-dep-blocks-path
  (is (= #{} (g/all-paths cyclic-dep-block :root :done))))
