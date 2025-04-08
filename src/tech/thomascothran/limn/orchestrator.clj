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
  Takes a sequence of effects and executes them.")

(defn- find-and-merge
  [finder data-requests]
  (reduce (fn [acc [query-name query-params]]
            (let [result (finder query-name query-params)]
              (if result
                (merge acc result)
                acc)))
          {}
          data-requests))

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

  Arity 2: Declare Effects, Events, and Anomalies
  -----------------------------------------------
  Takes the input (event or command) and the data.
  The results of the data request are merged into a single map and
  passed as the second argument to the decider function.

  Returns a map with the following keys:
  - `:effects` - A sequence of effects to execute
  - `:events` - The domain events to be emitted
  - `:anomaly/category` - an anomaly, if one has occured


  Other parameters to `m`
  ------------------------
  - `:finder` (query-name, opts): A function that takes the name
    of a query and options, returning data for that query.
  - `:workflow` (optional): the workflow
  - `:dispatch-effects!`: takes the sequence of effects and
    executes them
  "
  [m input]
  (let [dispatch-effects! (get m :dispatch-effects!)
        finder            (get m :finder)
        workflow          (get m :workflow)
        decider           (get m :decider)
        data-request      (get (decider input) :find)
        data              (find-and-merge finder data-request)
        result            (decider input data)
        effects           (get result :effects)
        events            (get result :events)
        anomaly           (get result :anomaly/category)
        run-effects?      (and effects (not anomaly))
        _                 (when run-effects?
                            (dispatch-effects! effects))]
    (when workflow
      (throw (Exception. "TODO")))
    (if anomaly
      (dissoc result :effects :events)
      {:effects effects
       :events events
       :data data})))
