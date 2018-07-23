## Chapter 3

_Most shift-reduce conflicts, and probably all reduce-reduce conflicts, should not be
resolved by fiddling with the parsing table. They are symptoms of an ill-specified
grammar, and they should be resolved by eliminating ambiguities._

### How to read parser output file

```
    state 0:
    items:
    _startstartProduction -> . startProduction

    actions:
    action ‘PLUS’ (noprec): error
	....
    action ‘POW’ (noprec): error
    action ‘IF’ (noprec): shift 6
    action ‘THEN’ (noprec): error
	...
	action ‘INT’ (noprec): shift 2
    action ‘error’ (noprec): error
    action ‘#’ (noprec): error
    action ‘$$’ (noprec): error

    immediate action:
    gotos:
    goto startProduction: 1
```
