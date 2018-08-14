## Chapter 3

_Most shift-reduce conflicts, and probably all reduce-reduce conflicts, should not be
resolved by fiddling with the parsing table. They are symptoms of an ill-specified
grammar, and they should be resolved by eliminating ambiguities._

`Dangling ELSE` is not a problem for LALR(1) parsers.

### FsYaccLex

When the rule and token have equal priority, then a `%left` precedence
favors **reducing**, `%right` favors **shifting**, and `%nonassoc` yields an error
action (do not allow same tokens to be repeated at the same level).

### Priority

`%left` to prefer **reduce** but how to order priority?

Let's say that `if..then..else` should be parsed. `then` should be with less
priority(to avoid reduce) because we want to continue for possible `else`.
Same for `for..to..do` and `while..do`. Indicate with `%left` that the construction
continues. But how to order all of them (`then`, `else` and `do`). Here is how I think:

 - `then` should be with less priority than `else` - do not hurry to reduce up to `then`
 - Is it possible to have `for/while......do..then/else`? - **NO**, but it is possible
   to have `...then/else..for/while...do` so we have to reduce `for/while` before move
   and finish with `if`. Conclusion is `do` has biggest priority, then `else` and with
   lower is `then`. Solution is `IF exp THEN exp %prec DO`.
