(ns tech.thomascothran.limn-test
  (:require [clojure.test :refer [deftest is]]
            [tech.thomascothran.limn.adapters]
            [tech.thomascothran.limn :as lm]))

(def ^:unit mow-lawn-spec
  {:workflow/name "Mow the lawn"
   :workflow/actions
   {:get-gas
    {:action/requires #{}
     :action/produces #{:mower/fueled}}

    :stop-running
    {:action/requires #{:mower/fueled
                        :mower/running}
     :action/produces #{[:not :mower/fueled]
                        [:not :mower/running]}}

    :start-mower
    {:action/requires #{:mower/fueled}
     :action/produces #{:mower/running}}

    :don-safety-glasses {:action/produces #{:worker/prepared}}

    :cut-grass
    {:action/requires #{:mower/running
                        :worker/prepared}
     :action/produces #{:grass/cut}}}})

(deftest make-workflow-action
  (is (= mow-lawn-spec
         (lm/make-workflow mow-lawn-spec))))

(comment
  (make-workflow-action))

(deftest find-facts
  (is (= #{:mower/fueled}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/add-facts #{:mower/fueled})
             (lm/facts)))))

(deftest find-action
  (is (-> (lm/make-workflow mow-lawn-spec)
          (lm/action :cut-grass))))

(deftest get-required-facts
  (is (= #{:mower/running :worker/prepared}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/requires :action :cut-grass)))))

(deftest action-complete?
  (is (-> (lm/make-workflow mow-lawn-spec)
          (lm/complete? :action :stop-running)))

  (is (-> (lm/make-workflow mow-lawn-spec)
          (lm/complete? :action :get-gas)
          (= false)))

  (is (-> (lm/make-workflow mow-lawn-spec)
          (lm/add-facts #{:mower/fueled})
          (lm/complete? :action :get-gas))))

(deftest all-complete-actions
  (is (= #{:stop-running}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/complete :actions))))

  (is (= #{:get-gas}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/add-facts #{:mower/fueled})
             (lm/complete :actions)))))

(deftest all-incomplete-actions
  (let [workflow-state (lm/make-workflow mow-lawn-spec)]
    (is (= (disj (set (keys (lm/actions workflow-state)))
                 :stop-running)
           (lm/incomplete workflow-state :actions)))))

(deftest find-actions
  (is (= #{:get-gas :start-mower :don-safety-glasses :cut-grass :stop-running}
         (set (keys (lm/actions (lm/make-workflow mow-lawn-spec)))))))

(deftest ready-actions
  (is (= #{:don-safety-glasses :get-gas}
         (lm/ready (lm/make-workflow  mow-lawn-spec) :actions)))
  (is (= #{:start-mower}
         (-> mow-lawn-spec
             lm/make-workflow
             (lm/add-facts #{:mower/fueled
                             :worker/prepared})
             (lm/ready :actions)))))

;; Negative conditions

(def negative-conditions-spec
  {:workflow/name "Negative conditions"
   :workflow/actions
   {:step-a {:action/requires #{}
             :action/produces #{:a}}
    :step-b {:action/requires #{[:not :a]}
             :action/produces #{:b}}}})

(deftest negative-conditions
  (is (= #{}
         (-> negative-conditions-spec
             lm/make-workflow
             (lm/complete :actions))))

  (is (= #{:step-a :step-b}
         (-> negative-conditions-spec
             lm/make-workflow
             (lm/incomplete :actions))))

  (is (= #{:step-a :step-b}
         (-> negative-conditions-spec
             lm/make-workflow
             (lm/ready :actions)))))

;; Repeatable actions

(def repeating-actions-spec
  {:workflow/name "Repeating actions"
   :workflow/actions
   {:step-a {:action/requires #{}
             :action/produces #{:a}}
    :step-b {:action/requires #{:a}
             :action/produces #{:b}
             :action/repeatable true}
    :step-c {:action/requires #{:a}
             :action/produces #{}
             :action/repeatable true}}})

(deftest repeatable-actions
  (is (= #{:step-a :step-b :step-c}
         (-> repeating-actions-spec
             lm/make-workflow
             (lm/incomplete :actions)))))

