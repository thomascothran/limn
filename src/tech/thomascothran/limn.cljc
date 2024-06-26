(ns tech.thomascothran.limn
  (:require [tech.thomascothran.limn.ports :as ports]
            [tech.thomascothran.limn.adapters]))

(defn make-workflow
  [workflow-spec]
  (ports/make-workflow workflow-spec))

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

(defn ready
  [workflow xtype]
  (ports/ready workflow xtype))

(defn authorized-actions
  [workflow actor]
  (ports/authorized-actions workflow actor))

(defn dependencies
  [workflow xtype xid]
  (throw (Exception. "TODO")))

(defn blockers
  [workflow action-id]
  (throw (Exception. "TODO")))

