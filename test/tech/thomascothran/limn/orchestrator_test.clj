(ns tech.thomascothran.limn.orchestrator-test
  (:require [clojure.test :refer [deftest is]]
            [tech.thomascothran.limn.orchestrator :as o]))

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
                   (assert (= :stub input))
                   {:find {:foo {:foo/id 1}
                           :bar {:bar/id 2}}})
                  ([input data]
                   (assert (= :stub input))
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
                :stub)]
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
                :stub)]
    (is (= {:anomaly/category :fault} result)
        "Should return anomaly, but not effects or events")))
