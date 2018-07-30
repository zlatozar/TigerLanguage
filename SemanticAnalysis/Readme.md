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


#### Type Checking Declarations

The effect of typing a declaration is to produce an extended environment:

```
typeDec : (A.dec * E.env) -> E.env

and typeDec (A.VarDec(vardec), env as (tenv, venv)) =
     (check that declared type (if any) and initialization expression are consistent)

| typeDec (A.FunctionDec(fundecs), (tenv, venv)) =
     (first pass: collect types of functions from headers and add bindings to env;
      second pass: type bodies of functions in environments extended with formal
      parameters, check consistency with return type.)

| typeDec (A.TypeDec(tydecs), (tenv, venv)) =
     (first pass: bind type ids to NAME(type_id, ref(NONE));
      second pass: translate definition and assign SOME(result) to ref;
      check for illegal cycles)
```
