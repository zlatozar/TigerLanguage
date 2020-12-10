## Parser

A parser is just a function that consumes less-structured input and produces more-structured output.
It is important because **data structure determines program structure.**

## Tiger Parser

Use to use language specification. Keep in mind that it is not complete so given test cases complete it.
If you create EBNF using some principles half of the work is done.

_Every grammar could be represented as a tree._

### Some generic guideline

- Parser looks one **token** ahead.

- Start from the specification and split to cases when there is a various cases.
  Can you distinguish between cases as looking one token ahead?

- Be careful with <...>* definitions when translate specification to parser definition!
  As you know <Dec>+ is <Dec> [<Dec>]*.

- `if..then` and `if..then..else` is a signal of **dangling else** problem.

- When use parser generator `left recursion` shouldn't be eliminated.

- Remember that the output of the lexer is the input for the parser so careful study the
  lexer tokens.

- Watch out for order in lists.

- Test very careful the generated tree. Every time read/check the resulted output.

- In specification sometimes there is _hidden_ clauses. In Tiger language this
  is `()`.

- As test use books examples. Is the output the same as in the book?

- Isolate border cases and test them separately.

#### Program

Tiger programs do not have arguments: a program is just an _expression_.

### EBNF

1. Read carefully the specification and outline the main(non terminal/long/complex)
   Tiger language expressions.

```
let <decs>* in <exp>* end
if <exp> then <exp> else <exp>
if <exp> then <exp>
while <exp> do <exp>
for (id := exp) to <exp> do <exp>
<exp> (binOp) <exp>
```

2. Start decompose them.

`let <decs>* in <exp>* end`

How many kinds of declarations we have: **type**(starts:TYPE), **variable**(starts: VAR)
and **function**(starts: FUNCTION) declarations. Do not forget that we could have many or
none declarations.

```
decs:
    | empty
    | tyDec decs
    | varDec decs
    | funDec decs
```
Because of mutual type/function declarations abstract syntax is more complex but
this will be fixed with embedded F# code. Continue with decomposition until only terminal
left.

```
tyDec: TYPE ID EQ ty

ty:
  | ID
  | LBRACE tyFields RBRACE
  | ARRAY OF ID

tyFields:
        | empty
        | ID COLON ID tyFieldsCont

// recursion is not a problem

tyFieldsCont:
            | empty
            | COMMA ID COLON ID tyFieldsCont
```

Again every 'case' could be distinguished by the first token. Do the same for **varDec**
and **funDec**.

3. Consider how to deal with nested expressions. Could this be source of confusion?

First of all we have to know when to stop and _reduce_ the expression. If you run the
parser generator first problems are because of the expression `typeId [<exp>] OF <exp>`
and `varId[<exp>]`. Both `typeId` and `varId` use ID token. How to distinguish them?
If next token (we look one token ahead) is `OF` do not reduce but continue - add `%right OF`.

Here priorities will help us. Here is an examples. Is it possible to have following nested
expression `while if <exp1> then <exp2> do <exp3>` (same with *FOR* cycle)? Answer is yes,
so `<exp2>` should be reduced upto `<do>` - `IF exp THEN exp %prec DO` (tricky).

4. Expand  `<exp> (binOp) <exp>` for every binary operation.

```
exp:
   | <exp> PLUS <exp>
   | <exp> MINUS <exp>
   ....
```
