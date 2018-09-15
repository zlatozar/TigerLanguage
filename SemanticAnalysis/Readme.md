## Semantic Analysis

#### Types.fs notes

1. `unique` is used to provide unique identities for declared record and array
types. These identities will be tested to determine type equality.

2. `NIL` and `UNIT` are used _internally_, and are not directly expressible in the source
code.

3. All type declarations cause type ids to be bound to `NAME` types, initially of form
`NAME(id, ref NONE)`, with the ref assigned the appropriate `(Some ty)` value in a second
pass.
