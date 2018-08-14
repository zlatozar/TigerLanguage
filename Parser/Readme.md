## Tiger Parser

Use to use language specification. Keep in mind that it is not complete so given test cases complete it.
If you create EBNF using some principles half of the work is done.

### Some generic guideline

-  Be careful with <...>* definitions when translate specification to parser definition!
   As you know <Dec>+ is <Dec> [<Dec>]*.

- `if..then` and `if..then..else` is the signal of **dangling else** problem.

- When use parser generator `left recursion` shouldn't be eliminated.

- Remember that the output of the lexer is the input for the parser so careful study the
  lexer tokens.

- Parser looks one **token** ahead.

- Watch out for order in lists.

- Test very careful with the generated tree. Every time read/check the resulted output.

- In specification sometimes there is _hidden_ clauses. In Tiger language this
  is `()`.

- As test use books examples. Is the output the same as in book?

- Isolate border cases and test them well.

#### Program

Tiger programs do not have arguments: a program is just an expression.

### EBNF