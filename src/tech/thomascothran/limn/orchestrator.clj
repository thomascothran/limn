(ns tech.thomascothran.limn.orchestrator
  "A dynamic orchestrator for workflows.

  Glues an imperative shell to a functional core.

  Functional Core
  ===============
  The functional core is a `decider` function that does two things:

  Declare data needs
  ------------------
  The imperative shell should be completely generic, knowing
  nothing about the domain. However, if the shell needs to know
  the data needs specific to a particular command, then it becomes
  tightly coupled to the core.

  The first arity of the `decider` function addresses this concern.

  Declare effects, events, and anomalies
  --------------------------------------
  The `decider` function makes *all* the decisions and encapsulates
  business rules.

  If no action can be taken on a command or an event, the `decider`
  function states what kind of anomaly has occurred and may provide
  additional data.

  Side effects are returned as data, and executed by the imperative shell.

  Events may also be returned, e.g., to be stored in a domain event log


  Arities
  -------
  The `decider` function has two arities:`

  - Arity 1: Takes an input (event or command) and returns a
    map with a `:find` key, which is a map of query names and
    options to fetch data.
  - Arity 2: Takes the input and the data fetched, and returns
    a map with the following keys:
    - `:effects` - A sequence of effects to execute
    - `:events` - The domain events to be emitted
    - `:anomaly/category` - an anomaly, if one has occured

  Imperative shell
  ================
  The imperative shell you will need to write is a function to
  fetch data, and a function to execute effects.


  `finder`
  --------
  The finder function takes a query name and options.

  For example, it might be called like this:

  ```clojure
  (finder :find-user {:user/name 'thomas'})
  ```

  `dispatch-effects`
  -----------------
  Takes a sequence of effects and executes them."
  (:require [tech.thomascothran.limn :as l]))

(defn- find-and-merge
  [finder data-requests]
  (reduce (fn [acc [query-name query-params]]
            (let [result (finder query-name query-params)]
              (if result
                (merge acc result)
                acc)))
          {}
          data-requests))

(defn workflow-anomalies
  [input workflow data]
  (when workflow
    (when-let [action-name (get input :action/name)]
      (let [workflow' (l/add-facts workflow data)
            available-actions (l/ready workflow' :actions)
            authorized-actions
            (into #{}
                  (map :action/name)
                  (l/authorized-actions workflow'))]
        (cond (not (get available-actions action-name))
              {:anomaly/category :conflict
               :blockers (l/blockers workflow' action-name)}

              (not (get authorized-actions action-name))
              {:anomaly/category :forbidden
               :personas (l/personas workflow')})))))

(defn- find-data
  [m input]
  (let [finder       (get m :finder)
        decider      (get m :decider)
        data-request (get (decider input) :find)]
    (find-and-merge finder data-request)))

(defn- orchestrate-step!
  [m input data]
  (or (workflow-anomalies input (get m :workflow) data)
      (let [dispatch-effects! (get m :dispatch-effects!)
            decider           (get m :decider)
            result            (decider input data)
            effects           (get result :effects)
            anomaly           (get result :anomaly/category)]
        (when (and effects (not anomaly))
          (dispatch-effects! effects))
        (if anomaly
          (dissoc result :effects :events)
          result))))

(defn- orchestrate-chain!
  [m input initial-data-supplied? initial-data]
  (loop [input'         input
         data-supplied? initial-data-supplied?
         supplied-data  initial-data
         effects        []
         events         []]
    (let [data    (if data-supplied?
                    supplied-data
                    (find-data m input'))
          result  (orchestrate-step! m input' data)
          anomaly (get result :anomaly/category)]
      (if anomaly
        (assoc result
               :effects effects
               :events events)
        (let [effects'    (into effects (get result :effects))
              events'     (into events (get result :events))
              next-action (get result :next-action)]
          (if next-action
            (recur next-action
                   (contains? result :next-state)
                   (get result :next-state)
                   effects'
                   events')
            {:effects effects'
             :events events'}))))))

(defn orchestrate!
  "Execute effects and emit events based on business logic.

  `input` is either an event or a command, represented as data.

  The `decider` function has two arities:

  Arity 1: Declare Data Needs
  ---------------------------
  Takes the input (event or command) and returns a
  map requesting the data it needs. The key of the map is the
  query name, and the value is the options. These are passed
  to the `finder` function.

  Arity 2: Declare Effects, Events, Anomalies, and Continuations
  ----------------------------------------------------------------
  Takes the input (event or command) and the data.
  The results of the data request are merged into a single map and
  passed as the second argument to the decider function.

  A result may contain `:next-action` to continue orchestration. If it also
  contains `:next-state`, that value is used as the complete data for the next
  action and its finder phase is skipped. Workflow rules are applied to every
  action in the chain.

  Returns a map with the effects and events aggregated across all actions:
  - `:effects` - A sequence of effects executed
  - `:events` - The domain events emitted
  - `:anomaly/category` - an anomaly, if one has occurred

  If an action produces an anomaly, orchestration stops. The returned effects
  and events contain only work completed by earlier actions in the chain.


  Other parameters to `m`
  ------------------------
  - `:finder` (query-name, opts): A function that takes the name
    of a query and options, returning data for that query.
  - `:workflow` (optional): the workflow
  - `:dispatch-effects!`: takes the sequence of effects and
    executes them
  "
  ([m input]
   (orchestrate-chain! m input false nil))
  ([m input data]
   (orchestrate-chain! m input true data)))

(defn adapt-single-data-request
  "Adapt a decider whose one-argument arity returns a single data request to
  the data-request contract used by `orchestrate!`.

  The adapted decider wraps that request in the `:find` structure expected by
  `orchestrate!`, using `::single-data-request` as its query name:

  ```clojure
  ((adapt-single-data-request decider) input)
  ;; => {:find {::single-data-request data-request}}
  ```

  Pair it with a finder that accepts the generated query name and handles the
  original request:

  ```clojure
  (fn [query-name data-request]
    (assert (= ::single-data-request query-name))
    (fetch-data data-request))
  ```

  As with any finder used by `orchestrate!`, the finder must return a map of
  facts. The decider's two-argument arity is delegated to unchanged. This
  function adapts only the decider; it does not adapt the finder or effect
  dispatcher."
  [decider]
  (fn
    ([input]
     {:find {::single-data-request (decider input)}})
    ([input data]
     (decider input data))))

(defn adapt-effects-persistence
  "Adapt a persistence function that accepts an `:effects` map for use as an
  `orchestrate!` `:dispatch-effects!` function.

  The returned function accepts a sequence of effects, wraps it in an
  `{:effects effects}` map, and passes that map to `persist!`. Its return value
  is the return value of `persist!`.

  This adapter handles effects only. It does not pass events, anomalies,
  continuations, or other decider-result attributes to `persist!`."
  [persist!]
  (fn [effects]
    (persist! {:effects effects})))

(defn ^:deprecated execute!
  "Deprecated. Use `orchestrate!`, which supports chained actions and returns
  anomalies (unlike execute!).

  Migration notes
  ---------------
  `execute!` and `orchestrate!` do not have identical contracts:

  - Replace `:fetch!` with `:finder`. The one-argument decider arity must return
    `{:find {query-name query-params}}`; the finder is called as
    `(finder query-name query-params)` and must return a map of facts. If the
    decider returns one data request directly, wrap it with
    `adapt-single-data-request` instead of changing the decider.
  - Replace `:persist!` with `:dispatch-effects!`. It receives only the sequence
    under `:effects`, rather than the complete decider result. If `persist!`
    handles only effects, wrap it with `adapt-effects-persistence`. Persist or
    publish returned events separately if the old `persist!` handled them.
  - Read events from `:events` in the returned map instead of using the complete
    return value as the event sequence.
  - Check `:anomaly/category` on the returned map. On an anomaly, `:effects` and
    `:events` contain only work completed by earlier actions in the chain; the
    anomalous action's effects are not dispatched.

  For example:

  ```clojure
  (let [result (orchestrate!
                {:decider (adapt-single-data-request decider)
                 :finder (fn [_query-name data-request]
                           (fetch! data-request))
                 :dispatch-effects!
                 (adapt-effects-persistence persist!)}
                action)]
    (if (:anomaly/category result)
      (handle-anomaly result)
      (:events result)))
  ```"
  [m action]
  (let [fetch! (get m :fetch!)
        persist! (get m :persist!)
        decider (get m :decider)]
    (loop [fetch-effects (decider action)
           state         nil
           action'       action
           events        []]
      (let [state' (or state (fetch! fetch-effects))
            persist-effects (decider action' state')
            next-action (get persist-effects :next-action)
            next-state  (get persist-effects :next-state)
            events' (into events (get persist-effects :events))]
        (persist! persist-effects)
        (if-not next-action
          events'
          (recur (if next-state
                   (decider action' next-state)
                   (decider action'))
                 next-state
                 next-action
                 events'))))))
