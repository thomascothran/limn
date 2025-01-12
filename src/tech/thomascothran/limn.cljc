(ns tech.thomascothran.limn
  (:require [tech.thomascothran.limn.ports :as ports]
            [tech.thomascothran.limn.adapters]
            [tech.thomascothran.limn.graph :as g]
            [clojure.set :as set]))

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
  [workflow]
  (ports/authorized-actions workflow))

(defn personas
  [workflow]
  (ports/personas workflow))

(defn- action-blockers
  ([workflow action-name]
   (let [ready-actions    (into #{}
                                (map :action/name)
                                (ready workflow :actions))
         complete-actions (complete workflow :actions)
         action-graph       (or (workflow :action-dependencies/graph)
                                (g/action-graph workflow))]
     (action-blockers workflow action-name {:ready-actions ready-actions
                                            :complete-actions complete-actions
                                            :action-graph action-graph})))
  ([_workflow action-name opts]
   (let [ready-actions (:ready-actions opts)
         complete-actions (:complete-actions opts)
         action-graph (:action-graph opts)
         processed-actions (:processed-actions opts)
         processed-action-names (into #{} (keys processed-actions))]
     (loop [action-queue    [action-name]
            already-checked #{}
            blockers        #{}]
       (if-let [action-name' (first action-queue)]
         (let [parents (get action-graph action-name' #{})
               ready-parents (set/intersection parents ready-actions)
               complete-parents (set/intersection parents complete-actions)
               known-parent-blockers (or (when processed-actions
                                           (->> (select-keys processed-actions
                                                             parents)
                                                (mapcat second)
                                                (into #{})))
                                         #{})

               already-checked'
               (reduce into already-checked
                       [ready-parents complete-parents
                        processed-action-names #{action-name'}])

               action-queue'
               (into (rest action-queue)
                     (comp (remove ready-parents)
                           (remove complete-parents)
                           (remove already-checked')
                           (remove processed-action-names))
                     parents)
               blockers' (reduce into blockers [ready-parents known-parent-blockers])]
           (recur action-queue'
                  already-checked'
                  blockers'))
         blockers)))))

(defn- workflow-blockers
  ([workflow]
   (let [ready-actions      (into #{}
                                  (map :action/name)
                                  (ready workflow :actions))
         complete-actions   (complete workflow :actions)
         incomplete-actions (incomplete workflow :actions)
         action-graph       (or (workflow :action-dependencies/graph)
                                (g/action-graph workflow))
         processed-actions  (->> (into ready-actions complete-actions)
                                 (reduce #(assoc %1 %2 #{}) {}))
         opts               {:action-graph action-graph
                             :ready-actions ready-actions
                             :complete-actions complete-actions
                             :incomplete-actions incomplete-actions
                             :processed-actions processed-actions}]

     (loop [queue (into [] incomplete-actions)
            processed-actions' processed-actions]
       (if-let [next-action-name (first queue)]
         (let [action-blockers'
               (action-blockers workflow next-action-name
                                (assoc opts :processed-actions
                                       processed-actions'))]
           (recur (rest queue)
                  (assoc processed-actions' next-action-name action-blockers')))
         processed-actions')))))

(defn blockers
  "Get the blockers for a workflow (single arity) or a
  specific action (2-arity)."
  ([workflow] (workflow-blockers workflow))
  ([workflow action-name] (action-blockers workflow action-name)))
