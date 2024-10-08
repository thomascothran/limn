(ns tech.thomascothran.limn.ports)

(defmulti make-workflow
  "Create a workflow"
  (fn [workflow-spec & _]
    (type workflow-spec)))

(defmulti add-facts
  (fn [workflow facts]
    [(type workflow) (type facts)]))

(defmulti facts
  (fn [workflow] (type workflow)))

(defmulti facts->set
  (fn [facts] (type facts)))

(defmulti actions
  "Find all the actions in a workflow"
  (fn [workflow & _]
    (type workflow)))

(defmulti complete?
  (fn [workflow xtype _id]
    [(type workflow) xtype]))

(defmulti requires
  (fn [workflow xtype _id]
    [(type workflow) xtype]))

(defmulti produces
  (fn [workflow xtype _id]
    [(type workflow) xtype]))

(defmulti action
  (fn [workflow _id] (type workflow)))

(defmulti complete
  (fn [workflow xtype]
    [(type workflow) xtype]))

(defmulti incomplete
  (fn [workflow xtype]
    [(type workflow) xtype]))

(defmulti ready
  (fn [workflow xtype]
    [(type workflow) xtype]))

(defmulti authorized-actions
  "What is the set of actions an actor (a human user or machine) is authorized to take?"
  (fn [workflow actor]
    [(type (:authorization/strategy workflow))
     (type actor)]))
