(ns tech.thomascothran.nexus.adapters
  (:require [clojure.set :as set]
            [tech.thomascothran.nexus.ports
             :as ports]))

(defn- tag
  [x tag]
  (vary-meta x assoc :type tag))

(defn- tag-workflow
  [workflow]
  (tag workflow :nexus/workflow))

(defn- tag-action
  [action]
  (tag action :nexus/action))

(defn tag-actions
  [actions]
  (reduce-kv (fn [m k v]
               (assoc m k (tag-action v)))
             {}
             actions))

(defn tag-facts
  [facts]
  (tag facts :nexus/fact-set))

(defmethod ports/make-facts
  :default
  [facts]
  (assert (set? facts))
  (tag-facts facts))

(defmethod ports/add-facts
  [:nexus/workflow :nexus/fact-set]
  [workflow facts]
  (assoc workflow :nexus/facts facts))

(defmethod ports/facts
  :nexus/workflow
  [workflow]
  (:nexus/facts workflow))

(defmethod ports/requires
  [:nexus/workflow :action]
  [workflow _ id]
  (-> (ports/action workflow id)
      (get :nexus.action/requires #{})))

(defmethod ports/produces
  [:nexus/workflow :action]
  [workflow _ id]
  (-> (ports/action workflow id)
      (get :nexus.action/produces #{})))

(defmethod ports/complete?
  [:nexus/workflow :action]
  [workflow _type action-id]
  (let [required-facts (ports/produces workflow :action action-id)
        facts (ports/facts workflow)]
    (set/subset? required-facts facts)))

(defmethod ports/make-workflow
  :default
  [workflow]
  (-> workflow
      (tag-workflow)
      (update :nexus.workflow/actions tag-actions)))

(defmethod ports/action
  :nexus/workflow
  [workflow action-id]
  (get-in workflow [:nexus.workflow/actions action-id]))

(defmethod ports/actions
  :default
  [workflow]
  (:nexus.workflow/actions workflow))

(defmethod ports/complete
  [:nexus/workflow :actions]
  [workflow _]
  (into #{}
        (filter #(ports/complete? workflow :action %))
        (keys (ports/actions workflow))))

(defmethod ports/incomplete
  [:nexus/workflow :actions]
  [workflow _]
  (into #{}
        (remove #(ports/complete? workflow :action %))
        (keys (ports/actions workflow))))

(defmethod ports/ready
  [:nexus/workflow :actions]
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


