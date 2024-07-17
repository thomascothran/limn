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

(defn action-graph
  [workflow]
  (->> (products->actions workflow)
       vals
       (reduce action-graph-reducer {})))
