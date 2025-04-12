(ns tech.thomascothran.limn.viz.mermaid-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [tech.thomascothran.limn.viz.mermaid :as m]
            [tech.thomascothran.limn :as l]))

(def football-actions
  [{:action/name :snap
    :viz/participant "center"}
   {:action/name :dropback
    :viz/participant "quarterback"}
   {:action/name :pass
    :viz/participant "quarterback"}
   {:action/name :catch
    :viz/participant "wide receiver"}
   {:action/name :tackle
    :viz/participant "corner"}])

(def expected-football-action-graph
  "sequenceDiagram
    center->>quarterback: :snap
    quarterback->>quarterback: :dropback
    quarterback->>wide receiver: :pass
    wide receiver->>corner: :catch
    corner->>corner: :tackle")

(defn- split-string-on-whitespace
  [s]
  (str/split s #"\s+"))

(deftest test-football-actions->sequence-diagram
  (let [diagram (m/actions->sequence-diagram football-actions)]
    (is (= (split-string-on-whitespace expected-football-action-graph)
           (split-string-on-whitespace diagram)))))
