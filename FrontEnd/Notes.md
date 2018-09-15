## Chapter 7

_Tree language_ instructions should be understood very well before start implementing translation.

#### Stm

- with `SEQ` could be created instruction sequences via **CONSing**.
  For example:

  ```
  SEQ
    (MOVE (TEMP 100,CONST 1),
     SEQ
       (LABEL ("L_0", 0),
        SEQ
          (MOVE (TEMP 100,CONST 2),
           SEQ (MOVE (TEMP 100,CONST 3),LABEL ("L_0", 0)))))
```

  It is not a good idea to write this code by hand. That's why in book is suggested to write a function
  to do that (also for readability):

```
instrChunk [ ...
             LABEL f;
               MOVE(TEMP r, CONST 0);
             LABEL t]
```

Tip: Read it as assembler code.

- `JUMP (exp, [labels])` calculate the address(or literal `NAME(l)`) for jump as evaluating the _exp_.
   Then match it to the list of given labels. By the way most used case is `JUMP(NAME (l), [l])`.

- `LABEL` - is like a label definition in assembly language. The value `NAME(h)` may be the target of jumps, calls, etc.
   Execution jumps to that label and start executing the code that _follows_. See previous example.

- `MOVE` is used to **store** results. That's way it has two forms:
   * `MOVE(TEMP r, exp)` evaluates the expression and store the result in _register r_.
   * `MOVE(MEM exp1, exp2)` First evaluate _exp1_ yielding address `addr`. Then evaluate _exp2_ and
     store the result into _wordSize_(defined in Frame interface) bytes of memory **starting** at `addr`.

- `EXP` evaluate given expression and **discards** the result.

#### Exp

- `MEM` when is used as the **left child** of a `MOVE`, it means _"store"_, but anywhere else it means **"fetch"** !

- `CALL(exp, params)` first execute `exp` to infer function name then calls it with given parameters.

- `ESEQ(stm, exp)` statement is evaluated for side effects (do not return value), then _exp_ is evaluated for a result.
