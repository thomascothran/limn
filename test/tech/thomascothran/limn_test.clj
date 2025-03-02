(ns tech.thomascothran.limn-test
  (:require [clojure.test :refer [deftest is testing]]
            [tech.thomascothran.limn.ports :as ports]
            [tech.thomascothran.limn.adapters]
            [tech.thomascothran.limn :as lm]))

(def mow-lawn-spec
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
  (is (= (assoc mow-lawn-spec :workflow/personas {})
         (lm/make-workflow mow-lawn-spec))))

(comment
  (make-workflow-action))

(deftest find-facts
  (is (= #{:mower/fueled}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/add-facts #{:mower/fueled})
             (lm/facts)))))

(deftest fact-set
  (is (= #{:mower/fueled}
         (ports/facts->set {:mower/fueled true})))
  (is (= #{:mower/fueled}
         (ports/facts->set {:mower/fueled false})))
  (is (= #{:mower/fueled}
         (ports/facts->set {:mower/fueled nil}))))

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
          (lm/add-facts {:mower/fueled true})
          (lm/complete? :action :get-gas))))

(deftest all-complete-actions
  (is (= #{:stop-running}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/complete :actions))))

  (is (= #{:get-gas}
         (-> (lm/make-workflow mow-lawn-spec)
             (lm/add-facts {:mower/fueled true})
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

  (let [ready-actions
        (-> mow-lawn-spec
            lm/make-workflow
            (lm/add-facts #{:mower/fueled
                            :worker/prepared})
            (lm/ready :actions))]
    (is (= #{:start-mower}
           ready-actions))))

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

  (let [ready-actions
        (-> negative-conditions-spec
            lm/make-workflow
            (lm/ready :actions))]
    (is (= #{:step-a :step-b}
           ready-actions))))

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

;; Blockers

(def blocker-spec
  {:workflow/actions
   {:blocker-a {:action/requires #{}
                :action/produces #{:a}}
    :blocker-b {:action/requires #{}
                :action/produces #{:z [:not :l]}}
    :non-blocker {:action/requires #{}
                  :action/produces #{:m}}
    :dangler     {:action/requires #{:x}
                  :action/produces #{:xy}}
    :intermediate {:action/requires #{:a}
                   :action/produces #{:b}}
    :negative {:action/requires #{[:not :l]
                                  [:not :b]}
               :action/produces #{[:not :n] :m}}
    :leaf {:action/requires #{:b :z}
           :action/produces #{:c}}}})

(deftest test-blockers
  (testing "Whether direct parents are identified as blockers"
    (let [workflow (lm/make-workflow blocker-spec)]
      (is (= #{}
             (lm/blockers workflow :blocker-a)
             (lm/blockers workflow :dangler)))
      (is (= #{:blocker-a}
             (lm/blockers workflow :intermediate)))))
  (testing "Whether grandparents are identified as blockers"
    (let [workflow (lm/make-workflow blocker-spec)]
      (is (= #{:blocker-a :blocker-b}
             (lm/blockers workflow :leaf))))))

(deftest test-negative-requirements-on-blockers
  (let [workflow (-> (lm/make-workflow blocker-spec)
                     (lm/add-facts {:a true}))]
    (is (= #{:blocker-b}
           (lm/blockers workflow :negative)))))

(deftest test-workflow-blockers
  (let [workflow (lm/make-workflow blocker-spec)]
    (is (= {:intermediate #{:blocker-a},
            :leaf #{:blocker-b :blocker-a}}
           (lm/blockers workflow)))))

(def complicated-blockers
  {:workflow/actions
   #:action{:redo-after-release
            #:action{:requires
                     #{:foo/granted
                       [:not
                        :foo/release-is-terminal]},
                     :produces
                     #{[:not :foo/granted]
                       [:not
                        :foo/pending-release]
                       :foo/pending-redo}}
            :grant-assigned-foo
            #:action{:requires
                     #{:foo/assigned
                       [:not :bar/stop]
                       :foo/id
                       [:not :foo/stop]},
                     :produces #{:foo/granted},
                     :personas
                     #{:persona/foo-manager
                       :persona/supervisor}},
            :create-foo
            #:action{:requires #{},
                     :produces
                     #{:foo/official-id
                       :foo/id}}
            :request-stop
            #:action{:requires
                     #{:foo/assigned
                       [:not :foo/sibling-stops]
                       [:not :foo/granted]
                       [:not :bar/stop]
                       [:not
                        :foo/pending-stop-request]
                       [:not :foo/cancelled]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/pending-stop-request}}
            :grant-stop-request
            #:action{:requires
                     #{:foo/pending-stop-request
                       [:not :foo/sibling-stops]
                       [:not :foo/granted]
                       [:not :bar/stop]
                       [:not :foo/cancelled]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/stop
                       [:not
                        :foo/pending-stop-request]}}
            :request-release
            #:action{:requires
                     #{[:not :foo/granted]
                       :foo/accepted
                       [:not
                        :foo/pending-release]
                       [:not
                        :foo/pending-assignment]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/pending-release
                       [:not
                        :foo/pending-request]
                       [:not
                        :foo/unconfirmed-info-provided]}}
            :delegate-foo
            #:action{:requires
                     #{[:not :foo/granted]
                       [:not :bar/stop]
                       :foo/id
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :repeatable true,
                     :produces
                     #{:foo/assigned
                       :foo/pending-assignment}}
            :bar-stop
            #:action{:requires #{},
                     :produces #{:bar/stop}}
            :require-redo
            #:action{:requires
                     #{:foo/pending-release},
                     :produces
                     #{[:not :foo/granted]
                       [:not
                        :foo/pending-release]
                       :foo/pending-redo}}
            :date-changed
            #:action{:requires
                     #{:foo/assigned
                       [:not :foo/granted]
                       [:not :foo/cancelled]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/pending-release-date-acceptance
                       :foo/date-changed},
                     :repeatable true}
            :accept-assigned-foo
            #:action{:requires
                     #{[:not :foo/granted]
                       [:not :bar/stop]
                       :foo/pending-assignment
                       :foo/id
                       [:not :foo/stop]},
                     :produces
                     #{:foo/accepted
                       [:not
                        :foo/pending-assignment]}}
            :stop
            #:action{:requires
                     #{[:not :foo/sibling-stops]
                       [:not :foo/granted]
                       [:not :bar/stop]
                       [:not
                        :foo/pending-stop-request]
                       [:not :foo/cancelled]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces #{:foo/stop}}
            :assign-foo
            #:action{:requires
                     #{[:not :bar/stop]
                       :foo/id
                       [:not :foo/cancelled]
                       [:not :foo/assigned]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/assigned
                       :foo/pending-assignment}}
            :provide-info
            #:action{:requires
                     #{[:not :foo/granted]
                       :foo/pending-request
                       :foo/id
                       [:not
                        :foo/pending-release]},
                     :produces
                     #{[:not
                        :foo/pending-request]
                       :foo/unconfirmed-info-provided}}
            :grant-own-foo
            #:action{:requires
                     #{:foo/accepted
                       [:not :bar/stop]
                       :foo/id
                       [:not :foo/stop]},
                     :produces #{:foo/granted}}
            :complete-redo
            #:action{:requires
                     #{:foo/accepted
                       [:not
                        :foo/pending-assignment]
                       :foo/pending-redo},
                     :produces
                     #{:foo/pending-release
                       [:not
                        :foo/pending-request]},
                     :personas
                     #{:persona/foo-assignee}},
            :unstop
            #:action{:requires #{:foo/stop},
                     :produces
                     #{[:not :foo/stop]}}
            :request-info
            #:action{:requires
                     #{:foo/assigned
                       [:not :foo/granted]
                       [:not
                        :foo/pending-request]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/pending-request},
                     :personas
                     #{:persona/foo-assignee}},
            :confirm-provided-information
            #:action{:requires
                     #{[:not :foo/granted]
                       [:not :bar/stop]
                       :foo/id
                       [:not
                        :foo/pending-request]
                       :foo/unconfirmed-info-provided
                       [:not :foo/stop]},
                     :produces
                     #{:foo/info-provided-accepted}}
            :schedule-date-changed
            #:action{:requires
                     #{[:not :foo/granted]
                       :foo/id
                       [:not :foo/cancelled]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/scheduled-date},
                     :repeatable true},
            :deny-stop-request
            #:action{:requires
                     #{:foo/pending-stop-request
                       [:not :foo/sibling-stops]
                       [:not :foo/granted]
                       [:not :bar/stop]
                       [:not :foo/cancelled]
                       [:not
                        :foo/pending-release]},
                     :produces
                     #{[:not
                        :foo/pending-stop-request]}}
            :accept-redo
            #:action{:requires
                     #{[:not :bar/stop]
                       [:not
                        :foo/redo-accepted]
                       :foo/id
                       [:not
                        :foo/pending-release]
                       :foo/pending-redo
                       [:not :foo/stop]},
                     :produces
                     #{:foo/redo-accepted},
                     :personas
                     #{:persona/foo-assignee}},
            :accept-release-date-change
            #:action{:requires
                     #{[:not :bar/stop]
                       :foo/id
                       [:not
                        :foo/release-date-change-accepted]
                       :foo/pending-release-date-acceptance
                       [:not :foo/stop]},
                     :produces
                     #{:foo/release-date-change-accepted},
                     :personas
                     #{:persona/foo-assignee}},
            :unrequest-info
            #:action{:requires
                     #{:foo/pending-request},
                     :produces
                     #{[:not
                        :foo/pending-request]}}
            :self-accept-foo
            #:action{:requires
                     #{[:not :bar/stop]
                       :foo/id
                       [:not
                        :foo/pending-release]
                       [:not :foo/assigned]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/accepted
                       [:not
                        :foo/pending-assignment]}}
            :reassign-foo
            #:action{:requires
                     #{:foo/assigned
                       [:not :foo/granted]
                       [:not :bar/stop]
                       :foo/id
                       [:not :foo/cancelled]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/pending-assignment
                       :foo/reassigned
                       :foo/owner-id}}
            :cancel
            #:action{:requires
                     #{[:not :foo/granted]
                       [:not :bar/stop]
                       [:not
                        :foo/pending-redo]
                       [:not
                        :foo/pending-release]
                       [:not :foo/stop]},
                     :produces
                     #{:foo/cancelled
                       [:not
                        :foo/unconfirmed-info-provided]}}
            :stop-on-sibling-foos
            #:action{:requires #{},
                     :produces
                     #{:foo/sibling-stops}}}})

(deftest test-complicated-blockers
  (let [workflow (-> complicated-blockers
                     (lm/add-facts {:foo/official-id 1
                                    :foo/id 2}))]
    (is (= #{:action/assign-foo :action/self-accept-foo :action/delegate-foo}
           (lm/blockers workflow :action/cancel)))))
