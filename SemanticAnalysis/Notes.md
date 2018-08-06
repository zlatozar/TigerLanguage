## Chapter 5

#### Guideline during development

- Format F# in a way that could be read 'at once'.

- Do baby development steps and test them.

- Use to use Tiger language - this will speed up test case writing.

- Look at the current Tiger language construction for example `AssignExp assignRec`.
  What information was collected in `assignRec` during parsing? Use it to prove
  that construction is correct from syntactic point of view.

- During semantic analysis all problems should be pointed so _dummy_ values
  should be returned to continue. Specify them.

- Try to understand the role of `actual type`. It is a central construction in
  this phase.

- Create a scaffold of the parsing AST function using given _PrettyPrint.fs_

- At the begging explicitly give type to all parameters and return types.

- There is no _boolean_ type. How to check `if/while/for` constructions?

- In list constructions during _fold_ initial types should be used. Specify them.

- Use helper functions for type comparison.

- Start with simple examples to test current development progress.

- Before start implementing logic it is a good idea to have
  marks that trace the path. Use `!` for implemented and `?` that are not.
  Here is mine:

```
...
| ForExp forRec -> printfn "    ?ForExp"; dummyTransExp
| LetExp letRec -> printfn "!LetExp"
                            ...
...
```
- Read careful the specification and track details. For example:
  * `if/then` should not return a value
  * (mutual) recursive types
  * (mutual) recursive functions
  * `break` nesting in a cycles `for/while`

- Be consistent in style and punctuation with meaningful error messages.

- How to proceed whit unit tests because only error messages will be returned?
