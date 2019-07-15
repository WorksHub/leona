# Leona
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

### Custom Scalars

[Custom scalars](https://lacinia.readthedocs.io/en/latest/custom-scalars.html) are also supported.

``` clojure
(-> (leona/create)
    ...
    (leona/attach-custom-scalar ::date {:parse     #(tf/parse (tf/formatters :date-time) %)
                                        :serialize #(tf/unparse (tf/formatters :date-time) %)}))
```

Anywhere the `::date` spec is referenced by an object, query or mutation, the `parse` and `serialize` fns will be used to transform the data. Be aware, however; whilst Leona will still perform its own internal spec validation, Lacinia will not perform validation for over-the-wire values. It's therefore important that your `parse` fn can handle incorrect data (unlike the one in the example!) and will still return something valid to Leona.



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

Sometimes, you may want to add a custom object that’s not referred to in any of your queries, mutations or field resolvers (e.g., if you want to refer to it from an external schema attached via `attach-schema`). For this use case, Leona provides `attach-object`:

```clojure
(-> (leona/create)
    ...
    (leona/attach-object :some/object :input? true))
```

If you pass `:input?`, as in the example above, Leona will generate an input object (named `object_input`) in addition to an ordinary object.

#### Type Aliases

If you're working with a large amount of legacy specs, sometimes you can have name clashes that aren't easy to resolve. To help with this you can use 'type aliases' which will automatically replace instances of _type_ names wherever they are used.

``` clojure
(-> (leona/create)
    ...
    (leona/attach-type-alias :my.ns/type :mytype)
```
In this example, if `my.ns/type` is an object, the corresponding object would be created as `:mytype` instead, and any references to `my.ns/type` would automatically be updated to use the alias instead. Note, this doesn't refer the _field_ names, just the _types_.

## Notes

The ordering of `attach-*` fns does not matter, other than for middleware.

## License

Copyright © 2018 Antony Woods, WorksHub Ltd.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
