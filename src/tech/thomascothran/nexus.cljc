(ns tech.thomascothran.nexus
  (:require [tech.thomascothran.nexus.ports :as ports]
            [tech.thomascothran.nexus.adapters]))

(defn make-workflow
  [workflow-spec]
  (ports/make-workflow workflow-spec))

(defn make-facts
  [facts]
  (ports/make-facts facts))

(defn add-facts
  [workflow facts]
  (ports/add-facts workflow facts))

(defn facts
  [workflow]
  (ports/facts workflow))

(defn action
  [workflow action-id]
  (ports/action workflow action-id))

(defn actions
  [workflow]
  (ports/actions workflow))

(defn complete?
  [workflow type id]
  (ports/complete? workflow type id))

(defn complete
  [workflow type]
  (ports/complete workflow type))

(defn incomplete
  [workflow type]
  (ports/incomplete workflow type))

(defn requires
  [workflow xtype xid]
  (ports/requires workflow xtype xid))

(defn produces
  [workflow xtype xid]
  (ports/produces workflow xtype xid))

(defn dependencies
  [workflow xtype xid]
  (throw (Exception. "TODO")))

(defn blockers
  [workflow action-id]
  (throw (Exception. "TODO")))
