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

(defn- eval*
  [x]
  #?(:clj (eval x)
     :cljs (if (fn? x) x (eval x))))

(defn- eval-vals
  [m]
  (into {} (map #(update % 1 eval*)) m))

(defn- derived-facts-applier
  [facts args]
  (let [rule-name (first args)
        rule (second args)
        rule-fn (eval* (get rule :fn))]
    [rule-name (boolean (rule-fn facts))]))

(defn- derived-facts
  [workflow facts]
  (let [rules (get workflow :workflow/rules)]
    (when rules
      (into {}
            (comp (map #(derived-facts-applier facts %))
                  (filter second))
            rules))))

(defmethod ports/add-facts
  [:clojure/map :clojure/map]
  [workflow facts]
  (->> (merge facts (derived-facts workflow facts))
       (assoc workflow :workflow/facts)))

(defmethod ports/add-facts
  [:clojure/map :clojure/set]
  [workflow facts]
  (assoc workflow :workflow/facts facts))

(defmethod ports/facts
  :clojure/map
  [workflow]
  (:workflow/facts workflow))

(defmethod ports/facts->set
  :clojure/map
  [fact-map]
  (set (keys fact-map)))

(defmethod ports/facts->set
  :clojure/set
  [fact-set]
  fact-set)

(defmethod ports/facts->set
  nil
  [_]
  #{})

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
        facts (or (ports/facts->set (ports/facts workflow))
                  #{})]

    (if repeatable?
      false
      (and (set/subset? required-products facts)
           (= #{} (set/intersection  excluded-facts facts))))))

(defmethod ports/make-workflow
  :clojure/map
  [workflow]
  (update workflow :workflow/personas eval-vals))

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
  (let [facts' (ports/facts->set facts)

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
  (let [facts  (ports/facts->set (ports/facts workflow))
        actions (ports/actions workflow)
        requirements #(ports/requires workflow :action %)
        incomplete-actions (ports/incomplete workflow :actions)]
    (into #{}
          (comp (filter (fn [[action-id _action]]
                          (action-ready? facts (requirements action-id))))
                (filter (comp incomplete-actions first))
                (map (fn [[action-name action]]
                       (assoc action :action/name action-name))))
          actions)))

(defmethod ports/ready
  [:clojure/map :actions]
  [workflow _actions]
  (into #{} (map :action/name) (ready workflow)))

(defmethod ports/personas
  :clojure/map
  [workflow]
  (if-let [personas (get workflow :workflow/personas)]
    (let [facts (get workflow :workflow/facts)
          apply-to-facts #(apply (second %) [facts])]
      (into #{}
            (comp (filter apply-to-facts)
                  (map first))
            personas))
    #{}))

(defmethod ports/authorized-actions
  [:clojure/map nil]
  [workflow]
  (let [ready-actions (ready workflow)
        personas (ports/personas workflow)]
    (into #{}
          (filter (fn [action]
                    (->> (get action :action/personas)
                         (set/intersection personas)
                         seq)))
          ready-actions)))
