(ns tech.thomascothran.limn-test
  (:require [clojure.test :refer :all]
            [tech.thomascothran.limn :as lm]))

(def ^:unit mow-lawn-spec
  {:limn.workflow/name "Mow the lawn"
   :limn.workflow/actions
   {:get-gas {:limn.action/requires #{}
              :limn.action/produces #{:mower/fueled}}
    :stop-running {:action/requires #{:mower/fueled
                                      :mower/running}
                   :action/produces #{[:not :mower/fueled]
                                      [:not :mower/running]}}
    :start-mower {:limn.action/requires #{:mower/fueled}
                  :limn.action/produces #{:mower/running}}
    :don-safety-glasses {:limn.action/produces #{:worker/prepared}}
    :cut-grass {:limn.action/requires #{:mower/running
                                        :worker/prepared}
                :limn.action/produces #{:grass/cut}}}})

(deftest make-workflow-action
  (is (= mow-lawn-spec
         (lm/make-workflow mow-lawn-spec)))

  (is (= :limn/workflow
         (type (lm/make-workflow mow-lawn-spec))))

  (is (= #{:limn/action}
         (->> mow-lawn-spec
              (lm/make-workflow)
              (lm/actions)
              vals
              (map type)
              (into #{})))))

(comment
  (make-workflow-action))

(deftest make-facts
  (is (= :limn/fact-set
         (type (lm/make-facts #{:a :b :c})))))

(deftest find-facts
  (is (= #{:mower/fueled}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/add-facts (lm/make-facts #{:mower/fueled}))
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
          (lm/complete? :action :get-gas)
          (= false)))

  (is (-> (lm/make-workflow mow-lawn-spec)
          (lm/add-facts (lm/make-facts #{:mower/fueled}))
          (lm/complete? :action :get-gas))))

(deftest all-complete-actions
  (is (= #{:stop-running}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/complete :actions))))

  (is (= #{:get-gas :stop-running}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/add-facts (lm/make-facts #{:mower/fueled}))
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
             (lm/add-facts (lm/make-facts #{:mower/fueled
                                            :worker/prepared}))
             (lm/ready :actions)))))

