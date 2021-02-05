## Chapter 3

_Most shift-reduce conflicts, and probably all reduce-reduce conflicts, should not be
resolved by fiddling with the parsing table. They are symptoms of an ill-specified
grammar, and they should be resolved by eliminating ambiguities._

`Dangling ELSE` is not a problem for LALR(1) parsers.

### FIRST and FOLLOW sets

`FIRST(A)` (or starters) is a set of terminal that could be at the begging of
the regular expression `A`. More formal: First sets are everything that stands
in the first position in a derivation. The tricky part is when start with
non-terminal. Let's clarify with examples:
```
FIRST(ε)   = {}
FIRST(t)   = {t}
FIRST(XY)  = FIRST(X)                 // If X do not generates 'epsilon'
FIRST(XY)  = FIRST(X) unite FIST(Y)   // If X generates 'epsilon'
FIRST(X|Y) = FIRST(X) unite FIST(Y)
FIRST(X*)  = FIRST(X)
```

`FOLLOW` sets are used when a symbol can be `ε` ("empty" symbol known as epsilon).
In a productions like: `A -> bXa`, if `X` can be `ε`, then it'll be eliminated,
and we still will be able to derive `a` which follows `X`. So we say that `a` is
in follow set of `X`.

In an LL(1) parser, the parser works by maintaining a workspace initially seeded
to the start symbol followed by the end-of-string marker (usually denoted `$`). At
each step, it does one of the following:

- If the first symbol of the workspace is a terminal, it matches it against the
  next token of input (or reports an error if it doesn't match.)

- If the first symbol of the workspace is a nonterminal, it predicts what
  production to replace that nonterminal with.

The predict step is where FIRST and FOLLOW show up. The parser needs to be able
to guess, based purely on the current nonterminal and the next token of input,
which production to use. The question is how to do this.

Let's suppose that the current nonterminal is `A` and the next token of input is
`t`. If you know the productions of `A`, which one would you choose to apply?
There's one simple case to consider: if there's a production of the form `A → tω`,
where `ω` is some arbitrary string, then you should pick that production because
the `t` you're looking at as input will match the `t` at the front of the
production.

There are also some complex cases to consider. Suppose you have a production of
the form `A → Bω`, where `B` is a **nonterminal** and `ω` is some _string_. Under what
circumstances would you want to guess this production? Well, if you know that
the next terminal symbol is a `t`, you wouldn't want to guess this production
unless you knew that `B` can expand to a string that starts with the terminal `t`
(there's another case that we'll talk about in a second). This is where FIRST
sets come in. In grammars without `ε` productions, the set FIRST(X) for some
nonterminal `X` is the set of all terminals that can potentially appear at the
start of some string derived from `X`. If you have a production of the form `A → Bω`
and you see the nonterminal `t`, you'd guess to use that production precisely when
`t ∈ FIRST(B)`; that is, `B` can derive some string that starts with `t`. If `B` doesn't
derive anything starting with `t`, then there's no reason to choose it, and if `B`
does derive something starting with `t`, you'd want to make this choice so that
you could eventually match the `t` against it.

Things get a bit trickier when `ε` productions are introduced. Now, let's suppose
that you have a production of the form `A → BCω`, where `B` and `C` are nonterminals
and `ω` is a string. Let's also suppose the next token of input is `t`. If `t ∈ FIRST(B)`,
then we'd choose this production, as mentioned above. However, what
happens if `t ∉ FIRST(B)`? If there are `ε` productions in the grammar, we might
still want to choose this production if `B` can derive `ε` and `t ∈ FIRST(C)`. Why is
this? If this happens, it means that we might be able to match the `t` by
producing `BCω`, then producing `ε` from `B`, leaving `Cω` against which to match the `t`.
This is one context where we might have to **"look through"** a nonterminal.
Fortunately, this is handled by FIRST sets. If a nonterminal `X` can produce `ε`,
then `ε ∈ FIRST(X)`. Therefore, we can use FIRST sets to check whether we need to
**"look through"** a nonterminal by seeing whether `ε ∈ FIRST(X)`.

So far we haven't talked about FOLLOW sets. Where do they come in? Well, suppose
that we're processing the nonterminal `A`, we see the terminal `t`, but none of the
productions for `A` can actually consume the `t`. What do we do then? It turns out
there's still a way that we can _eat up_ that `t`. Remember that LL(1) parsers work
by maintaining a workspace with a string in it. It's possible that the `t` we're
looking at is not supposed to be matched against the current nonterminal `A` at
all, and instead we're supposed to have `A` produce `ε` and then let some later
nonterminal in the workspace match against the `t`. This is where FOLLOW sets come
in. The FOLLOW set of a nonterminal `X`, denoted `FOLLOW(X)`, is the set of all
terminal symbols that can appear _immediately after `X`_ in some derivation. When
choosing which production to choose for `A`, we add in a final rule - if the
terminal symbol `t` is in the FOLLOW set of `A`, we choose some production that
ultimately will produce `ε`. That way, the `A` will **"disappear"** and we can match the
`t` against some character that appears after the A nonterminal.

[Compilers course - slides](https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/)

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
