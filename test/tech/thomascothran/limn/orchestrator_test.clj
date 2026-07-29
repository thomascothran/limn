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
    (is (= [effect] (first @!state)))))

(deftest test-adapt-single-data-request
  (let [input        {:action/name :load-widget}
        data-request {:widget/id 42}
        facts        {:widget/id 42
                      :widget/status :ready}
        decision     {:events [{:event/type :widget/loaded}]}
        decider      (fn
                       ([input']
                        (assert (= input input'))
                        data-request)
                       ([input' facts']
                        (assert (= input input'))
                        (assert (= facts facts'))
                        decision))
        adapted      (o/adapt-single-data-request decider)]
    (is (= {:find {::o/single-data-request data-request}}
           (adapted input))
        "The single request should use the orchestrator's :find structure")
    (is (= decision (adapted input facts))
        "The decision arity should be delegated to unchanged")))

(deftest test-adapt-effects-persistence
  (let [!persisted (atom nil)
        effects    [{:effect/type :widget/save
                     :widget/id 42}]
        persist!   (fn [result]
                     (reset! !persisted result)
                     :persisted)
        dispatch!  (o/adapt-effects-persistence persist!)]
    (is (= :persisted (dispatch! effects))
        "The persistence function's return value should be preserved")
    (is (= {:effects effects} @!persisted)
        "Effects should be presented using the persistence map contract")))

(deftest text-execute!-with-continuous-actions
  (let [!state (atom [])

        log-foo-action {:action/type :foo/log-foo}
        log-bar-action {:action/type :bar/log-bar}

        foo {:foo/id 1 :foo/status :ready}

        bar {:bar/id 2 :bar/status :closed}

        fetch! (fn [{event-type :event/type}]
                 (case event-type
                   :find-foo foo
                   :find-bar bar))

        persist! (fn [effects]
                   (swap! !state conj effects))

        event1 {:event/type :found-foo :state foo}
        event2 {:event/type :found-bar :state bar}

        decider (fn decider
                  ([{action-type :action/type}]
                   (case action-type
                     :foo/log-foo {:event/type :find-foo}
                     :bar/log-bar {:event/type :find-bar}))
                  ([{action-type :action/type :as action} state]
                   (case action-type
                     :foo/log-foo
                     {:effects [{:effect/type :log-foo
                                 :action action
                                 :state state}]
                      :next-action log-bar-action
                      :events [event1]}

                     :bar/log-bar
                     {:effects [{:effect/type :log-bar
                                 :action action
                                 :state state}]
                      :events [event2]})))
        result (o/execute!
                {:fetch! fetch!
                 :persist! persist!
                 :decider decider}
                log-foo-action)]

    (is (= [{:effect/type :log-foo
             :state foo
             :action log-foo-action}]
           (get (first @!state) :effects)))

    (is (= [event1 event2] result))))

(deftest test-orchestrate!-with-continuous-actions
  (let [!finder-calls (atom [])
        !dispatched-effects (atom [])

        prepare-action {:action/name :prepare}
        process-action {:action/name :process}
        finish-action {:action/name :finish}

        prepare-effect {:effect/type :work/prepare}
        process-effect {:effect/type :work/process}
        finish-effect {:effect/type :work/finish}

        prepared-event {:event/type :work/prepared}
        processed-event {:event/type :work/processed}
        finished-event {:event/type :work/finished}

        workflow (lm/make-workflow
                  {:workflow/name "Continuous work"
                   :workflow/actions
                   {:prepare
                    {:action/requires #{}
                     :action/produces #{:work/prepared}}

                    :process
                    {:action/requires #{:work/prepared}
                     :action/produces #{:work/processed}}

                    :finish
                    {:action/requires #{:work/processed}
                     :action/produces #{:work/finished}}}})

        finder (fn [query-name query-params]
                 (swap! !finder-calls conj [query-name query-params])
                 (case query-name
                   :find-prepare {}
                   :find-finish {:work/processed true}))

        decider (fn decider
                  ([{action-name :action/name}]
                   (case action-name
                     :prepare {:find {:find-prepare {}}}
                     :process (throw (ex-info "Process state should be supplied"
                                              {:action/name action-name}))
                     :finish {:find {:find-finish {}}}))
                  ([{action-name :action/name} data]
                   (case action-name
                     :prepare
                     (do
                       (assert (= {} data))
                       {:effects [prepare-effect]
                        :events [prepared-event]
                        :next-action process-action
                        :next-state {:work/prepared true}})

                     :process
                     (do
                       (assert (= {:work/prepared true} data))
                       {:effects [process-effect]
                        :events [processed-event]
                        :next-action finish-action})

                     :finish
                     (do
                       (assert (= {:work/processed true} data))
                       {:effects [finish-effect]
                        :events [finished-event]}))))

        result (o/orchestrate!
                {:dispatch-effects! #(swap! !dispatched-effects conj %)
                 :finder finder
                 :decider decider
                 :workflow workflow}
                prepare-action)]

    (is (= [prepare-effect process-effect finish-effect]
           (get result :effects)))
    (is (= [prepared-event processed-event finished-event]
           (get result :events)))
    (is (= [[prepare-effect] [process-effect] [finish-effect]]
           @!dispatched-effects))
    (is (= [[:find-prepare {}] [:find-finish {}]]
           @!finder-calls))))

(deftest test-workflow-is-enforced-for-next-action
  (let [!decided-actions (atom [])
        !dispatched-effects (atom [])

        start-action {:action/name :start}
        finish-action {:action/name :finish}
        start-effect {:effect/type :work/start}
        started-event {:event/type :work/started}

        workflow (lm/make-workflow
                  {:workflow/name "Blocked continuation"
                   :workflow/actions
                   {:start
                    {:action/requires #{}
                     :action/produces #{:work/started}}

                    :unlock
                    {:action/requires #{}
                     :action/produces #{:work/unlocked}}

                    :finish
                    {:action/requires #{:work/unlocked}
                     :action/produces #{:work/finished}}}})

        decider (fn decider
                  ([{action-name :action/name}]
                   {:find {(case action-name
                             :start :find-start
                             :finish :find-finish)
                           {}}})
                  ([{action-name :action/name} _data]
                   (swap! !decided-actions conj action-name)
                   (case action-name
                     :start {:effects [start-effect]
                             :events [started-event]
                             :next-action finish-action}
                     :finish (throw (ex-info "Blocked action was decided"
                                             {:action/name action-name})))))

        result (o/orchestrate!
                {:dispatch-effects! #(swap! !dispatched-effects conj %)
                 :finder (fn [_query-name _query-params] {})
                 :decider decider
                 :workflow workflow}
                start-action)]

    (is (= {:anomaly/category :conflict
            :blockers #{:unlock}
            :effects [start-effect]
            :events [started-event]}
           result))
    (is (= [:start] @!decided-actions))
    (is (= [[start-effect]] @!dispatched-effects))))

(deftest test-anomaly-in-next-action-preserves-completed-work
  (let [first-action {:action/name :first}
        second-action {:action/name :second}
        first-effect {:effect/type :work/first}
        rejected-effect {:effect/type :work/rejected}
        first-event {:event/type :work/first-completed}
        rejected-event {:event/type :work/rejected}

        decider (fn decider
                  ([_] {:find {}})
                  ([{action-name :action/name} _data]
                   (case action-name
                     :first {:effects [first-effect]
                             :events [first-event]
                             :next-action second-action}
                     :second {:anomaly/category :fault
                              :reason :second-action-failed
                              :effects [rejected-effect]
                              :events [rejected-event]})))

        result (o/orchestrate!
                {:dispatch-effects! (constantly nil)
                 :finder (fn [_query-name _query-params] {})
                 :decider decider}
                first-action)]

    (is (= {:anomaly/category :fault
            :reason :second-action-failed
            :effects [first-effect]
            :events [first-event]}
           result))))

(deftest text-execute!-with-continuous-actions-with-state
  (let [!state (atom [])

        log-foo-action {:action/type :foo/log-foo}
        log-bar-action {:action/type :bar/log-bar}

        foo {:foo/id 1 :foo/status :ready}

        bar {:bar/id 2 :bar/status :closed}

        fetch! (fn [{event-type :event/type}]
                 (case event-type
                   :find-foo foo
                   :find-bar bar))

        persist! (fn [effects]
                   (swap! !state conj effects))

        event1 {:event/type :found-foo :state foo}
        event2 {:event/type :found-bar :state bar}

        decider (fn decider
                  ([{action-type :action/type}]
                   (case action-type
                     :foo/log-foo {:event/type :find-foo}))
                  ([{action-type :action/type :as action} state]
                   (case action-type
                     :foo/log-foo
                     {:effects [{:effect/type :log-foo
                                 :action action
                                 :state state}]
                      :next-action log-bar-action
                      :next-state bar
                      :events [event1]}

                     :bar/log-bar
                     {:effects [{:effect/type :log-bar
                                 :action action
                                 :state state}]
                      :events [event2]})))
        result (o/execute!
                {:fetch! fetch!
                 :persist! persist!
                 :decider decider}
                log-foo-action)]

    (is (= [{:effect/type :log-foo
             :state foo
             :action log-foo-action}]
           (get (first @!state) :effects)))

    (is (= [event1 event2] result))))

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
    (is (= {:anomaly/category :fault
            :effects []
            :events []}
           result)
        "Should return the anomaly with no completed work")))

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
