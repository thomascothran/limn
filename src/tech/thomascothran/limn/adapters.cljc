(ns tech.thomascothran.limn.adapters
  (:require [clojure.set :as set]
            [tech.thomascothran.limn.ports
             :as ports]))

#?(:clj (do (derive clojure.lang.PersistentArrayMap :clojure/map)
            (derive clojure.lang.PersistentHashMap :clojure/map))
   :cljs (do (derive PersistentArrayMap :clojure/map)
             (derive PersistentHashMap :clojure/map)))

#?(:clj (derive clojure.lang.PersistentHashSet :clojure/set)
   :cljs (derive PersistentHashSet :clojure/set))

(defmethod ports/add-facts
  [:clojure/map :clojure/map]
  [workflow facts]
  (assoc workflow :workflow/facts facts))

(defmethod ports/add-facts
  [:clojure/map :clojure/set]
  [workflow facts]
  (assoc workflow :workflow/facts facts))

(defmethod ports/facts
  :clojure/map
  [workflow]
  (:workflow/facts workflow))

(defmethod ports/requires
  [:clojure/map :action]
  [workflow _ id]
  (-> (ports/action workflow id)
      (get :action/requires #{})))

(defmethod ports/produces
  [:clojure/map :action]
  [workflow _ id]
  (-> (ports/action workflow id)
      (get :action/produces #{})))

(defmethod ports/complete?
  [:clojure/map :action]
  [workflow _type action-id]
  (let [produces
        (ports/produces workflow :action action-id)

        {repeatable? :action/repeatable}
        (ports/action workflow action-id)

        required-products
        (into #{} (filter keyword?) produces)

        excluded-facts
        (into #{}
              (comp (filter #(and (vector? %)
                                  (= :not
                                     (first %))))
                    (map second))
              produces)
        facts (or (ports/facts workflow)
                  #{})]

    (if repeatable?
      false
      (and (set/subset? required-products facts)
           (= #{} (set/intersection  excluded-facts facts))))))

(defmethod ports/make-workflow
  :clojure/map
  [workflow]
  workflow)

(defmethod ports/action
  :clojure/map
  [workflow action-id]
  (get-in workflow [:workflow/actions action-id]))

(defmethod ports/actions
  :clojure/map
  [workflow]
  (:workflow/actions workflow))

(defmethod ports/complete
  [:clojure/map :actions]
  [workflow _]
  (into #{}
        (filter #(ports/complete? workflow :action %))
        (keys (ports/actions workflow))))

(defmethod ports/incomplete
  [:clojure/map :actions]
  [workflow _]
  (into #{}
        (comp (remove #(and (not (get (second %)
                                      :limn.action/repeatable))
                            (ports/complete? workflow :action (first %))))
              (map first))
        (ports/actions workflow)))

(defn- action-ready?
  [facts requirements]
  (let [facts' (into #{} facts)

        positive-requirements
        (into #{} (filter keyword) requirements)

        negative-requirements
        (into #{} (comp (filter #(and (vector? %) (= :not (first %))))
                        (map second))
              requirements)
        positive-is-subset? (set/subset? positive-requirements facts')]
    (and positive-is-subset?
         (= #{}
            (set/intersection negative-requirements facts')))))

(defn- ready
  [workflow]
  (let [facts (ports/facts workflow)
        actions (ports/actions workflow)
        requirements #(ports/requires workflow :action %)
        incomplete-actions (ports/incomplete workflow :actions)]
    (into #{}
          (comp (filter (fn [[action-id _action]]
                          (action-ready? facts (requirements action-id))))
                (filter (comp incomplete-actions first))
                (map first))
          actions)))

(defmethod ports/ready
  [:clojure/map :actions]
  [workflow _actions]
  (ready workflow))

