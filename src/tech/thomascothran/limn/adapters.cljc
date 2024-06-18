(ns tech.thomascothran.limn.adapters
  (:require [clojure.set :as set]
            [tech.thomascothran.limn.ports
             :as ports]))

(defn- tag
  [x tag]
  (vary-meta x assoc :type tag))

(defn- tag-workflow
  [workflow]
  (tag workflow :limn/workflow))

(defn- tag-action
  [action]
  (tag action :limn/action))

(defn tag-actions
  [actions]
  (reduce-kv (fn [m k v]
               (assoc m k (tag-action v)))
             {}
             actions))

(defn- make-fact-map
  [m]
  (-> (reduce-kv (fn [m k v]
                   (if (nil? v)
                     m
                     (assoc m k v)))
                 ^{:type :limn/fact-map} {}
                 m)
      (tag :limn/fact-map)))

#?(:clj (defmethod ports/make-facts
          clojure.lang.PersistentHashMap
          [facts]
          (make-fact-map facts)))

#?(:clj (defmethod ports/make-facts
          clojure.lang.PersistentArrayMap
          [facts]
          (make-fact-map facts)))

#?(:cljs (defmethod ports/make-facts
           PersistentArrayMap
           [facts]
           (make-fact-map facts)))

(defmethod ports/add-facts
  [:limn/workflow :limn/fact-set]
  [workflow facts]
  (assoc workflow :limn/facts facts))

(defmethod ports/facts
  :limn/workflow
  [workflow]
  (:limn/facts workflow))

(defmethod ports/requires
  [:limn/workflow :action]
  [workflow _ id]
  (-> (ports/action workflow id)
      (get :limn.action/requires #{})))

(defmethod ports/produces
  [:limn/workflow :action]
  [workflow _ id]
  (-> (ports/action workflow id)
      (get :limn.action/produces #{})))

(defmethod ports/complete?
  [:limn/workflow :action]
  [workflow _type action-id]
  (let [required-facts (ports/produces workflow :action action-id)
        facts (ports/facts workflow)]
    (set/subset? required-facts facts)))

(defmethod ports/make-workflow
  :default
  [workflow]
  (-> workflow
      (tag-workflow)
      (update :limn.workflow/actions tag-actions)))

(defmethod ports/action
  :limn/workflow
  [workflow action-id]
  (get-in workflow [:limn.workflow/actions action-id]))

(defmethod ports/actions
  :default
  [workflow]
  (:limn.workflow/actions workflow))

(defmethod ports/complete
  [:limn/workflow :actions]
  [workflow _]
  (into #{}
        (filter #(ports/complete? workflow :action %))
        (keys (ports/actions workflow))))

(defmethod ports/incomplete
  [:limn/workflow :actions]
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

(comment
  (action-ready? #{:a :b :c} #{:a :b :c})
  (action-ready? #{:a :b} #{:a :b :c})
  (action-ready? #{:a :b} #{:a [:not :b]})
  (action-ready? nil #{[:not :a]}))

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
  [:limn/workflow :actions]
  [workflow _]
  (ready workflow))


