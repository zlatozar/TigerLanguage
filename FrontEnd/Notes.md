## Chapter 6

**Frame** is used for a variety of purposes:

  - To hold values passed to a procedure as arguments
  - To save registers that a procedure may modify, but which the procedure's caller does
    not want changed
  - To provide space for variables local to a procedure

The frame pointer `$fp` points to the first word in the currently executing procedure's
stack frame.  The stack pointer `$sp` points to the last word of frame. The first four
arguments are passed in registers, so the fifth argument is the first one stored on the
stack.

The steps below describe the calling convention used on most **MIPS** machines.

#### Immediately before the caller invokes the callee

1. _Pass arguments_. By convention, the first four arguments are passed in registers
`$a0-$a3`. Any remaining arguments are pushed on the stack and appear at the beginning of
the called procedure's stack frame.

2. _Save caller-saved registers_. The called procedure can use these registers (`$a0-$a3`
and `$t0-$t9`) without first saving their value. If the caller expects to use one of these
registers after a call, it must save its value before the call.

3. _Execute a_ `jal` _instruction_, which jumps to the callee's first instruction and
saves the return address in register `$ra`.

#### Just as the callee starts executing

1. Allocate memory for the frame by subtracting the frame's size from the stack pointer.

2. Save callee-saved registers in the frame. A callee must save the values in these
registers (`$s0-$s7`, `$fp`, and `$ra`) before altering them since the caller expects to
find these registers unchanged after the call. Register `$fp` is saved by every procedure
that allocates a new stack frame. However, register `$ra` only needs to be saved if the
callee itself makes a call. The other calleesaved registers that are used also must be
saved.

3. Establish the frame pointer by adding the stack frame's size **minus 4** to `$sp` and
storing the sum in register `$fp`.

#### Immediately before the callee returns to the caller

1. If the callee is a function that returns a value, place the returned value in register
`$v0`.

2. Restore all callee-saved registers that were saved upon procedure entry.

3. Pop the stack frame by adding the frame size to `$sp`.

4. Return by jumping to the address in register `$ra`.

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
