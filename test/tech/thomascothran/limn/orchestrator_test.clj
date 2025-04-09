(ns tech.thomascothran.limn.orchestrator-test
  (:require [clojure.test :refer [deftest is]]
            [tech.thomascothran.limn.orchestrator :as o]
            [tech.thomascothran.limn :as lm]))

(deftest test-orchestrator
  (let [!state (atom [])
        dispatch-effects! #(swap! !state conj %)
        finder (fn [query-name m]
                 (cond (and (= :foo query-name)
                            (= 1 (get m :foo/id)))
                       {:foo/id 1
                        :foo/status "ready"}

                       (and (= :bar query-name)
                            (= 2 (get m :bar/id)))
                       {:bar/id 2
                        :bar/status "closed"}
                       :else (throw (ex-info "Not found"
                                             {:query-name query-name
                                              :m m}))))
        effect {:effect/type :foo/activate
                :foo/id 1}

        events [{:type :foo/activated
                 :foo/id 1}]

        decider (fn decider
                  ([input]
                   (assert (= {:event/type :stub} input))
                   {:find {:foo {:foo/id 1}
                           :bar {:bar/id 2}}})
                  ([input data]
                   (assert (= {:event/type :stub} input))
                   (assert (= {:foo/id 1
                               :foo/status "ready"
                               :bar/id 2
                               :bar/status "closed"}
                              data))
                   {:effects [effect]
                    :events events}))

        result (o/orchestrate!
                {:dispatch-effects! dispatch-effects!
                 :finder finder
                 :decider decider}
                {:event/type :stub})]
    (is (= [effect] (get result :effects))
        "Should return effects")
    (is (= events (get result :events))
        "Should return events")
    (is (= {:foo/id 1
            :foo/status "ready"
            :bar/id 2
            :bar/status "closed"}
           (get result :data))
        "Should return the data")
    (is (= [effect] (first @!state)))))

(deftest test-anomalies
  (let [!state (atom [])

        dispatch-effects! #(swap! !state conj %)

        finder (fn [query-name {id :foo/id}]
                 (when (and (= :foo query-name)
                            (= 1 id))
                   {:foo/id 1
                    :foo/status "ready"}))

        decider (fn decider
                  ([_] {:find {:foo {:foo/id 1}}})
                  ([_ _] {:anomaly/category :fault
                          :events [{:type :abc}]
                          :effects [{:effect/type :foo/activate
                                     :foo/id 1}]}))

        result (o/orchestrate!
                {:dispatch-effects! dispatch-effects!
                 :finder finder
                 :decider decider}
                {:event/type :stub})]
    (is (= {:anomaly/category :fault} result)
        "Should return anomaly, but not effects or events")))

(def test-persona-workflow
  (lm/make-workflow
   {:workflow/name "Delegate"
    :workflow/actions
    {:assign-task
     {:action/requires #{}
      :action/produces #{:task/assigned-to-id}
      :action/personas #{:supervisor}}

     :reassign-task
     {:action/requires #{:task/assigned-to-id}
      :action/produces #{:task/reassigned-at}
      :action/personas #{:delegator}}

     :perform-task
     {:action/requires #{:task/assigned-to-id}
      :action/produces #{:task/completed-at}
      :action/personas #{:worker :supervisor}}

     :approve-task
     {:action/requires #{:task/performed}
      :action/produces #{:task/approved}
      :action/personas #{:supervisor}}}

    :workflow/personas
    {:worker :user/is-worker
     :supervisor '(fn [facts]
                    ((comp :supervisor :user/roles) facts))
     :delegator '(fn [{user-id :user/id
                       delegators :delegator-list}]
                   (-> (into #{} delegators)
                       (get user-id)))}}))

(deftest test-whether-workflow-is-enforced
  (let [input {:action/name :reassign-task}
        config {:finder #(throw (ex-info "don't call" %&))
                :decider #(throw (ex-info "don't call" %&))
                :workflow test-persona-workflow}

        data {:user/roles #{:supervisor}}
        result (o/orchestrate! config input data)]
    (is (= :conflict (get result :anomaly/category)))
    (is (= #{:assign-task} (get result :blockers)))))

(deftest test-whether-personas-are-enforced
  (let [input {:action/name :assign-task}
        config {:finder #(throw (ex-info "don't call" %&))
                :decider #(throw (ex-info "don't call" %&))
                :workflow test-persona-workflow}

        data {:user/is-worker true}
        result (o/orchestrate! config input data)]
    (is (= :forbidden (get result :anomaly/category)))
    (is (= #{:worker} (get result :personas))))

  (let [input {:action/name :assign-task}
        config {:decider (constantly {})
                :workflow test-persona-workflow}

        data {:user/roles #{:supervisor}}
        result (o/orchestrate! config input data)]
    (is (nil? (get result :anomaly/category)))))
