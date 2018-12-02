# Leona
_Pen or sword - the shield is mightiest  - Leona_

[![Clojars Project](https://img.shields.io/clojars/v/leona.svg)](https://clojars.org/leona)
[![CircleCI](https://circleci.com/gh/acron0/leona/tree/master.svg?style=svg)](https://circleci.com/gh/acron0/leona/tree/master)

A toolbox designed to make working with GraphQL and clojure.spec a more pleasant experience.

Leona can build Lacinia schema just by telling it the queries and mutations you want to make. You can add resolvers for specific fields and add middleware inside the executor.

## Usage

``` clojure
(require '[leona.core :as leona])

(let [schema (-> (leona/create)
                 (leona/attach-query ::query-spec ::object query-resolver-fn)
                 (leona/attach-mutation ::mutation-spec ::object mutator-fn)
                 (leona/attach-field-resolver ::field-in-object field-resolver-fn)
                 (leona/attach-middleware middeware-fn)
                 (leona/compile))]
  (leona/execute schema "query { object(id: 1001) { id, name, field_in_object }}")
```

## License

Copyright © 2018 Antony Woods, WorksHub Ltd.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
