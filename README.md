# limn.clj: Artifact-Oriented Business Process Modeling

Define you who can do what, when, and why in the context of a business process using data:

```clojure
(def mow-lawn-spec
  {:workflow/name "Mow the lawn"
   :workflow/actions
   {:get-gas
    {:action/requires #{}
     :action/produces #{:mower/fueled}}

    :stop-running
    {:action/requires #{:mower/fueled
                        :mower/running}
     :action/produces #{[:not :mower/fueled]
                        [:not :mower/running]}}

    :start-mower
    {:action/requires #{:mower/fueled}
     :action/produces #{:mower/running}}

    :don-safety-glasses {:action/produces #{:worker/prepared}}

    :cut-grass
    {:action/requires #{:mower/running
                        :worker/prepared}
     :action/produces #{:grass/cut}}}})

```

## Purpose

Limn is designed to model business processes inspired by artifact-centric business process modeling. It aims to answer critical questions about business workflows, such as who can perform what actions and under what conditions, based on the current state of business entities.

## Core Problem

The primary objective of Limn is to extract business rules -- specifically who can perform what actions, when, and why -- from business logic, application logic, and IO. By doing so, Limn seeks to provide a clear separation between business rules and other layers of the application, facilitating easier maintenance and understanding of business processes.

In many business applications, business rules tend to get mixed in with business logic, application logic, and even the IO layer. This intermingling makes it difficult to manage and evolve business processes without introducing errors or inconsistencies. Limn addresses this problem by providing a structured approach to defining and managing business rules separately.

## Artifact-Centric Business Process Modeling (ACBPM)

Traditional BPMN-style approaches pose several challenges:

- Complexity: The comprehensive nature of BPMN can lead to overly complex diagrams, especially for large-scale processes, making them hard to manage and understand.
- Versioning Issues: Managing changes and versions in BPMN can be challenging, particularly when regulatory or business requirements evolve, necessitating changes to the flow.
- Sequential Focus: BPMN's emphasis on the sequence of activities can make it less flexible in handling scenarios where the focus is on the state of business entities rather than the sequence of steps.

ACBPM focuses instead the "artifacts" or key business entities and their lifecycles. Artifacts represent business-relevant objects, such as orders, invoices, or claims, and the model defines the states these artifacts pass through and the actions that cause state transitions.

Advantages:

- Simplicity and Clarity: By concentrating on artifacts and their states, ACBPM provides a clear and straightforward way to understand what actions are permissible at any point in time based on the current state.
- Version Management: ACBPM avoids complex versioning issues. Since actions are defined in terms of current states rather than sequences of events, changes in requirements (e.g., adding new regulatory steps) are easier to manage.
- Comparison and Analysis: The artifact-centric approach allows for easy comparison and diffing of workflows, making it straightforward to evaluate different scenarios or detect inconsistencies.
- Data-Driven: This approach lends itself well to data-driven modeling, which can be easily serialized and manipulated, supporting automation and integration with other systems.

## Design Goals

### Data-Driven Approach

Limn uses a data-driven approach to declaratively define business processes. This approach ensures that workflows are easy to read, serialize, version, and manage. By default, Limn emphasizes a maximally declarative methodology, allowing business processes to be represented in a straightforward and consistent manner as a Clojure map.

### Extensibility

While the data-driven approach covers many use cases, Limn also recognizes the need for flexibility. Not every scenario fits neatly into a predefined data structure. Therefore, Limn supports extensibility through ad hoc runtime polymorphism. This means you can create custom modeling processes and even store rules in a database if necessary.
Artifact-Centric Business Process Modeling

Limn adopts artifact-centric business process modeling for several reasons:

    Simplicity: It avoids the complexity of determining allowed actions based on previously performed actions, which can lead to significant versioning issues. For instance, if regulatory requirements change the sequence of actions, it becomes challenging to track the exact state and version of each action in traditional process-centric models.

    Version Management: By focusing on the current state of an entity, artifact-centric modeling sidesteps the issues of tracking different versions of actions and their effects.

    Comparison and Analysis: Artifact-oriented models make it straightforward to compare different workflows, analyze potential future scenarios, run hypothetical scenarios, and identify discrepancies.

### Dependencies and Blocking Actions

Identifying dependencies between actions is crucial for managing workflows effectively. In complex workflows, multiple paths may exist from start to finish. Limn provides the capability to programmatically identify blockers, helping to resolve issues that cause workflows to stall. Understanding these dependencies allows for smoother progression and better management of business processes.

## Prior Art

- Dativity
- Titanaboa

## License

Copyright © 2024 Default

_EPLv1.0 is just the default for projects generated by `clj-new`: you are not_
_required to open source this project, nor are you required to use EPLv1.0!_
_Feel free to remove or change the `LICENSE` file and remove or update this_
_section of the `README.md` file!_

Distributed under the Eclipse Public License version 1.0.
