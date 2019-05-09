# Leona 2
_Pen or sword - the shield is mightiest  - Leona_

[![Clojars Project](https://img.shields.io/clojars/v/workshub/leona.svg)](https://clojars.org/workshub/leona) [![CircleCI](https://circleci.com/gh/WorksHub/leona.svg?style=svg)](https://circleci.com/gh/WorksHub/leona)

A toolbox designed to make working with GraphQL and clojure.spec a more pleasant experience.

Leona can build Lacinia schema just by telling it the queries and mutations you want to make. You can add resolvers for specific fields and add middleware inside the executor.

## Quick Usage

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

## Examples

### Queries

To add a query to the schema use `attach-query`:

```clojure
(-> (leona/create)
    (leona/attach-query ::query-spec ::object query-resolver-fn))
```
`::query-spec` is a spec for the GraphQL query, `::object` is the spec for the returned data, and `query-resolver-fn` is the resolver function that will fetch and return the data.

### Mutations

Mutations are very similar to queries. To add a mutation to the schema use `attach-mutation`:

```clojure
(-> (leona/create)
    (leona/attach-mutation ::mutation-spec ::object mutator-fn))
```
`::mutation-spec` is a spec for the GraphQL mutation, `::object` is the spec for the returned data, and `mutator-fn` is the function that will mutate the existing data and return the new, mutated data.

### Field Resolvers

To provide a resolver for a specific field, use `attach-field-resolver`:

```clojure
(-> (leona/create)
    (leona/attach-query ::query-spec ::object query-resolver-fn)
    (leona/attach-field-resolver ::field-in-object field-resolver-fn)
```

`::field-in-object` is a spec for the field in an existing object. **It must match a field already being inserted, in either a query or mutation. If the field isn't found amongst the objects in the schema then it won't be inserted.** `field-resolver-fn` is a resolver fn for that specific field. As is true of all field resolvers, it will be called *after* the root query/mutation resolver, so the `value` arg will already have data in it. The field resolver should add to this value.

### Middleware

It might be useful to add middleware *inside* the Lacinia executor e.g. you want to inspect a query/mutation prior to resolving **or** you want to inspect a value before it's passed back to Lacinia.

```clojure
(-> (leona/create)
    (leona/attach-middleware middeware-fn-1)
    (leona/attach-middleware middeware-fn-2))
```

Middleware functions are applied in the order that they are attached and take 4 args:

```clojure
(defn middleware-fn-1
[handler ctx query value]
  ;; do something
  (handler))

(defn middleware-fn-2
[handler ctx query value]
  (let [result (handler)]
  ;; do something
  result)
```

Middleware should call `(handler)` if they intend to allow the process to continue. Currently the `ctx`, `query` and `value` args should not be passed into `handler` as they cannot be overridden by the middleware.

In order to use the middleware you'll need to use leona's `execute` fn:

```clojure
(leona/execute compiled execute-string)
```

`compiled` is the output of `(leona/compile)` and `execute-string` is a GraphQL query/mutation/etc.

### Other

If your spec cannot be inferred (automatically converted into an accurate schema) you can always override the inferred type by using the `spec` function from `spec-tools`:

```clojure
(require '[spec-tools.core :as st])

(s/def ::object (st/spec object? {:type 'String}))
```

If you'd like to add a description to the schema you can also use the `spec` function:

```clojure
(s/def ::object (st/spec string? {:description "This is my object}))
```

## Notes

The ordering of `attach-*` fns does not matter, other than for middleware.

## License

Copyright © 2018 Antony Woods, WorksHub Ltd.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
