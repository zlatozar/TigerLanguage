## Semantic Analysis

#### Types

1. `unique` is used to provide unique identities for declared record and array
types. These identities will be tested to determine type equality.

2. `NIL` and `UNIT` are used _internally_, and are not directly expressible in the source
code

3. All type declarations cause type ids to be bound to `NAME` types, initially of form
`NAME(id, ref NONE)`, with the ref assigned the appropriate `(Some ty)` value in a second
pass.

4. The `ERROR` type is used to represent the type of an erroneous expression, and plays a
role in error recovery
