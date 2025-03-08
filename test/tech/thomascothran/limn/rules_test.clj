(ns tech.thomascothran.limn.rules-test
  (:require [clojure.test :refer [deftest testing is]]
            [tech.thomascothran.limn :as lm]
            [tech.thomascothran.limn.adapters]))

(def make-coffee-workflow
  {:workflow/name "Make Coffee"
   :workflow/actions
   {:get-beans {:action/requires #{}
                :action/produces #{:sufficient-coffee-beans}}
    :grind {:action/requires #{:sufficient-coffee-beans}
            :action/produces #{:ground-coffee}}
    :brew {:action/requires #{:ground-coffee}
           :action/produces #{:coffee}}
    :pour {:action/requires #{:coffee}
           :action/produces #{:coffee-cup}}}

   :workflow/rules
   {:sufficient-coffee-beans
    {:fn '(fn [facts] (<= 3 (get facts :scoops)))}}})

(deftest test-rules-application
  (is (= #{:get-beans}
         (-> (lm/make-workflow make-coffee-workflow)
             (lm/add-facts {:scoops 2})
             (lm/ready :actions))))

  (is (= #{:grind}
         (-> (lm/make-workflow make-coffee-workflow)
             (lm/add-facts {:scoops 3})
             (lm/ready :actions))))

  (is (= #{:grind}
         (-> make-coffee-workflow
             (assoc-in [:workflow/actions :workflow/rules :sufficient-coffee-beans]
                       (constantly true))
             (lm/make-workflow)
             (lm/add-facts {:scoops 3})
             (lm/ready :actions)))))

(deftest test-rules-revocation
  (is (-> (lm/make-workflow make-coffee-workflow)
          (lm/add-facts {:scoops 3})
          (lm/ready :actions))))
