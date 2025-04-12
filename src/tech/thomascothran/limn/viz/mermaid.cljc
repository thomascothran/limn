(ns tech.thomascothran.limn.viz.mermaid
  (:require [clojure.string :as str]))

(defn- actions->sequence-elements
  [actions]
  (mapv (fn [{:viz/keys [participant arrow]
              action-name :action/name
              :or {arrow "->>"}}
             {next-participant :viz/participant}]
          (str participant arrow (or next-participant
                                     participant)
               ": " action-name))
        actions
        (conj (into [] (rest actions)) nil)))

(defn actions->sequence-diagram
  [actions]
  (str "sequenceDiagram\n"
       (str/join "\n"
                 (actions->sequence-elements actions))))
