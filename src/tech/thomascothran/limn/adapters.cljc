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

(defn tag-facts
  [facts]
  (tag facts :limn/fact-set))

(defmethod ports/make-facts
  :default
  [facts]
  (assert (set? facts))
  (tag-facts facts))

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
        (remove #(ports/complete? workflow :action %))
        (keys (ports/actions workflow))))

(defmethod ports/ready
  [:limn/workflow :actions]
  [workflow _]
  (let [facts (ports/facts workflow)
        actions (ports/actions workflow)
        requirements #(ports/requires workflow :action %)
        incomplete-actions (ports/incomplete workflow :actions)]
    (into #{}
          (comp (filter (fn [[action-id _action]]
                          (set/subset? (requirements action-id)
                                       facts)))
                (filter (comp incomplete-actions first))
                (map first))
          actions)))


