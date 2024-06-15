(ns tech.thomascothran.nexus-test
  (:require [clojure.test :refer :all]
            [tech.thomascothran.nexus :as nx]))

(def ^:unit mow-lawn-spec
  {:nexus.workflow/name "Mow the lawn"
   :nexus.workflow/actions
   {:get-gas {:nexus.action/requires #{}
              :nexus.action/produces #{:mower/fueled}}
    :stop-running {:action/requires #{:mower/fueled
                                      :mower/running}
                   :action/produces #{[:not :mower/fueled]
                                      [:not :mower/running]}}
    :start-mower {:nexus.action/requires #{:mower/fueled}
                  :nexus.action/produces #{:mower/running}}
    :don-safety-glasses {:nexus.action/produces #{:worker/prepared}}
    :cut-grass {:nexus.action/requires #{:mower/running
                                         :worker/prepared}
                :nexus.action/produces #{:grass/cut}}}})

(deftest make-workflow-action
  (is (= mow-lawn-spec
         (nx/make-workflow mow-lawn-spec)))

  (is (= :nexus/workflow
         (type (nx/make-workflow mow-lawn-spec))))

  (is (= #{:nexus/action}
         (->> mow-lawn-spec
              (nx/make-workflow)
              (nx/actions)
              vals
              (map type)
              (into #{})))))

(comment
  (make-workflow-action))

(deftest make-facts
  (is (= :nexus/fact-set
         (type (nx/make-facts #{:a :b :c})))))

(deftest find-facts
  (is (= #{:mower/fueled}
         (-> (nx/make-workflow mow-lawn-spec)
             (nx/add-facts (nx/make-facts #{:mower/fueled}))
             (nx/facts)))))

(deftest find-action
  (is (-> (nx/make-workflow mow-lawn-spec)
          (nx/action :cut-grass))))

(deftest get-required-facts
  (is (= #{:mower/running :worker/prepared}
         (-> (nx/make-workflow mow-lawn-spec)
             (nx/requires :action :cut-grass)))))

(deftest action-complete?
  (is (-> (nx/make-workflow mow-lawn-spec)
          (nx/complete? :action :get-gas)
          (= false)))

  (is (-> (nx/make-workflow mow-lawn-spec)
          (nx/add-facts (nx/make-facts #{:mower/fueled}))
          (nx/complete? :action :get-gas))))

(deftest all-complete-actions
  (is (= #{:stop-running}
         (-> (nx/make-workflow mow-lawn-spec)
             (nx/complete :actions))))

  (is (= #{:get-gas :stop-running}
         (-> (nx/make-workflow mow-lawn-spec)
             (nx/add-facts (nx/make-facts #{:mower/fueled}))
             (nx/complete :actions)))))

(deftest all-incomplete-actions
  (let [workflow-state (nx/make-workflow mow-lawn-spec)]
    (is (= (set (keys (nx/actions workflow-state)))
           (nx/incomplete workflow-state :actions)))))

(deftest find-actions
  (is (= #{:get-gas :start-mower :don-safety-glasses :cut-grass}
         (set (keys (nx/actions (nx/make-workflow mow-lawn-spec)))))))

(deftest ready-actions
  (is (= #{:don-safety-glasses :get-gas}
         (nx/ready (nx/make-workflow  mow-lawn-spec) :actions)))
  (is (= #{:start-mower}
         (-> mow-lawn-spec
             nx/make-workflow
             (nx/add-facts (nx/make-facts #{:mower/fueled
                                            :worker/prepared}))
             (nx/ready :actions)))))

