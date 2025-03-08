(ns tech.thomascothran.limn.persona-test
  (:require [clojure.test :refer [deftest is]]
            [tech.thomascothran.limn :as lm]))

(def test-persona-workflow
  {:workflow/name "Delegate"

   :workflow/actions
   {:assign-task
    {:action/requires #{}
     :action/produces #{:task/assigned-to-id}
     :action/personas #{:supervisor}}

    :reassign-task
    {:action/requires #{:task/assigned-to-id}
     :action/produces #{:task/reassigned-at
                        :task/delegator-id}
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
                      (get user-id)))}})

(deftest test-whether-personas-are-activated
  (is (= #{}
         (-> (lm/make-workflow test-persona-workflow)
             (lm/add-facts {:user/is-worker false
                            :user/id 1
                            :delegator-list [2]})
             (lm/personas))))

  (is (= #{:worker}
         (-> (lm/make-workflow test-persona-workflow)
             (lm/add-facts {:user/is-worker true
                            :user/id 1
                            :delegator-list [2]})
             (lm/personas))))

  (is (= #{:worker :delegator}
         (-> (lm/make-workflow test-persona-workflow)
             (lm/add-facts {:user/is-worker true
                            :user/id 1
                            :delegator-list [1]})
             (lm/personas))))
  (is (= #{:supervisor}
         (-> (lm/make-workflow test-persona-workflow)
             (lm/add-facts {:user/roles #{:supervisor}
                            :user/id 1
                            :delegator-list [2]})
             (lm/personas)))))

(deftest test-whether-actions-are-restricted
  (let [workflow
        (-> (lm/make-workflow test-persona-workflow)
            (lm/add-facts {:user/is-worker false
                           :user/id 1
                           :delegator-list [2]}))
        authorized-actions (lm/authorized-actions workflow)]
    (is (= #{} authorized-actions)))

  (let [workflow
        (-> (lm/make-workflow test-persona-workflow)
            (lm/add-facts {:user/roles #{:supervisor}
                           :user/id 1
                           :delegator-list [2]}))
        authorized-actions (lm/authorized-actions workflow)
        expected #{#:action{:requires #{}
                            :produces #{:task/assigned-to-id},
                            :personas #{:supervisor},
                            :name :assign-task}}]
    (is (= expected authorized-actions))))
