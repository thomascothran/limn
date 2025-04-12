(ns tech.thomascothran.limn.graph
  (:require [tech.thomascothran.limn.ports
             :as ports]))

(defn associate-action-with-deps
  "Assocate an action with its dependencies.

  `m` is a map of a product to the actions that require
  and produce it. E.g.,

  ```
  {:coffee-brewed {:product/producers #{:use-keurig :use-french-press}
                   :produces/requirers #{:drink-coffee} }}
  ```"
  [workflow
   action-name
   product->action-m]

  (let [requirements (ports/requires workflow
                                     :action
                                     action-name)

        products (ports/produces workflow
                                 :action
                                 action-name)

        add-reqs
        (fn add-reqs [m']
          (reduce (fn [acc requirement]
                    (update-in acc [requirement :product/requirers]
                               #(into #{action-name} %)))
                  m'
                  requirements))
        add-produces
        (fn add-produces [m']
          (reduce (fn [acc product]
                    (update-in acc [product :product/producers]
                               #(into #{action-name} %)))
                  m'
                  products))]
    ((comp add-reqs add-produces) product->action-m)))

(defn products->actions
  "get a map of the products to their related
  producing and requiring actions.

  Example return value:

  ```
  {:coffee-brewed {:product/producers #{:use-keurig :use-french-press}
                   :produces/requirers #{:drink-coffee} }}
  ```"
  [workflow]
  (let [action-names (keys (ports/actions workflow))]
    (reduce (fn [acc action-name]
              (associate-action-with-deps
               workflow action-name acc))
            {}
            action-names)))

(defn- set-parents
  [dep-graph child parents]
  (reduce (fn [acc parent]
            (update acc child #(into #{parent} %)))
          dep-graph
          parents))

(defn- create-edges
  [dep-graph requirers producers]
  (reduce (fn [acc requirer]
            (set-parents acc requirer producers))
          dep-graph
          requirers))

(defn- action-graph-reducer
  [dep-graph
   {:product/keys [producers requirers]
    :or {producers #{}
         requirers #{}}}]

  (create-edges dep-graph requirers producers))

(defn children->parents
  [workflow]
  (->> (products->actions workflow)
       vals
       (reduce action-graph-reducer {})))

(defn parents->children
  "Map of the parents (providers of facts) to their immediate
  chilren (actions that require those facts)"
  [workflow]
  (let [products->actions' (products->actions workflow)]
    (reduce (fn [acc actions]
              (let [producers (get actions :product/producers #{})
                    requires  (get actions :product/requirers #{})]
                (reduce (fn [acc' producer]
                          (update acc' producer (partial into requires)))
                        acc
                        producers)))

            {}
            (vals products->actions'))))

(comment
  (parents->children
   {:workflow/actions
    {:root {:action/requires #{}
            :action/produces #{:a}}
     :middle {:action/requires #{:a}
              :action/produces #{:b}}
     :leaf {:action/requires #{:b}
            :action/produces #{:c}}
     :island {:action/requires #{:y}
              :action/produces #{:z}}}}))

(defn- roots
  [workflow]
  (into #{}
        (comp
         (remove (comp seq
                       #(ports/requires workflow :action
                                        (first %))))
         (map first))
        (ports/actions workflow)))

(defn action->ancestors
  "returns all the dependencies for a given action, both
  the immediate parent actions that provide its required facts,
  and the dependencies for those parents, grandparents, etc."
  [workflow]
  (let [parents->children (parents->children workflow)]
    (loop [queue               (roots workflow)
           action->ancestors   (into {}
                                     (comp (map first)
                                           (map (fn [action-name]
                                                  [action-name #{}])))
                                     (ports/actions workflow))
           processed           #{}]
      (if-let [action-name (first queue)]
        (let [children (get parents->children action-name #{})
              queue' (into (disj queue action-name)
                           (remove processed)
                           children)
              action->ancestors'
              (reduce (fn [acc child]
                        (update acc child into
                                (conj
                                 (get action->ancestors action-name #{})
                                 action-name)))
                      action->ancestors
                      children)
              processed' (conj processed action-name)]
          (recur queue' action->ancestors' processed'))
        action->ancestors))))

(comment
  (time
   (action->ancestors
    {:workflow/actions
     {:root {:action/requires #{}
             :action/produces #{:a}}
      :middle {:action/requires #{:a}
               :action/produces #{:b}}
      :leaf {:action/requires #{:b}
             :action/produces #{:c}}
      :island {:action/requires #{:y}
               :action/produces #{:z}}}})))

(defn all-paths
  "Get all non-cyclic paths from one node to another.

  Caution: this can increase exponentially."
  [workflow from to]
  (let [checked (transient #{})
        graph (parents->children workflow)
        paths (transient #{})]
    (loop [path [from]
           stack (into (list) (get graph from))]
      (when-let [next-action (first stack)]
        (cond (get checked next-action)
              (recur path (rest stack))

              (= next-action to)
              (do (conj! paths (conj path to))
                  (recur (into [] (butlast path))
                         (rest stack)))
              :else
              (do (conj! checked next-action)
                  (recur (conj path next-action)
                         (into (rest stack) (get graph next-action)))))))
    (persistent! paths)))
