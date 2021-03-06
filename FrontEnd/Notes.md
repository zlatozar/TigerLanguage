## Chapter 6

_Describes the data structures that should be created on top of memory and registers._
_First data should be placed on frame or register(Frame.Access) then we must define a way to access the data (Stm)._

In this and next chapter we work only with _addresses_ - calculate or modify and then
translate to a Tree language.

**Frame** is used for a variety of purposes:

  - To hold values passed to a procedure as arguments
  - To save registers that a procedure may modify, but which the procedure's caller does
    not want changed
  - To provide space for variables local to a procedure

The frame pointer(MIPS) `$fp` points to the first word in the **currently executing** procedure's
stack frame.  The stack pointer `$sp` points to the last word of frame. The first four
arguments are passed in registers, so the fifth argument is the first one stored on the
stack.

The steps below describe the calling convention used on most **MIPS** machines.

Tip: Function calling conventions try to ensure to generate as few memory traffic as possible.

#### Registers

We know the location in the frame for _arguments_ and that _result_ is in `RV`. But how to proceed with registers?
They are common for caller and the callee and the main task is to prevent them from **overwriting** each other's regiters.

**Caller-saved** registers (volatile registers) are used to hold temporary quantities that need not be preserved across calls.
For that reason, it is the caller's responsibility to push these registers onto the stack if it wants to restore this value after
a procedure call.

**Callee-saved** registers (non-volatile registers) are used to hold long-lived values that should be preserved across calls.
When the caller makes a procedure call, it can expect that those registers will hold the same value after the callee returns,
making it the _responsibility of the callee_ to save them and restore them before returning to the caller. Callee function
should do some extra work.

For example let say that we enter a function `f(int a)`. Function could use all registers for local variables to be operations fast.
If there is call to another function `g()` all the register should be saved. When you call a function, you should assume that
everything currently in your registers will be wiped out. This is overhead so it is good idea to define which one to save before
call because they are needed after the `g()` returns. That's way register are divided.

#### Immediately before the caller invokes the callee (function starts)

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

3. Establish the frame pointer by adding the stack frame's size **minus WORDSIZE** to `$sp` and
storing the sum in register `$fp`.

#### Immediately before the callee returns to the caller (function exit)

1. If the callee is a function that returns a value, place the returned value in register `$v0`.

2. Restore all callee-saved registers that were saved upon procedure entry.

3. Pop the stack frame by adding the frame size to `$sp`.

4. Return by jumping to the address in register `$ra`.

## Chapter 7

Describes the steps that should be made to transform AST to IR using the data structures(frames)
defined in previous chapter.

_Tree language_ instructions should be understood very well before start implementing translation.

NOTE: IR tree is a program representation; can be executed directly by an interpreter. Execution
is a tree traversal and jumps to labels.

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
           SEQ (MOVE (TEMP 100, CONST 3),LABEL ("L_0", 0)))))
```

  It is not a good idea to write this code by hand. That's why in book is suggested to write a function
  to do that (also for readability):

```
blockCode [ ...
             LABEL f;
               MOVE(TEMP r, CONST 0);
             LABEL t]
```

### Intermediate Representation (IR)

- `JUMP (exp, [labels])` calculate the address(or literal `NAME(l)`) for jump as evaluating the _exp_.
   Then match it to the list of given labels. By the way most used case is `JUMP(NAME (l), [l])`.

- `LABEL(n)` - is like a label definition in assembly language. Define the constant value of name `n`
   to be the current machine code address. Execution jumps to that label and start executing the code that _follows_.
   Look at `NAME(label)` as a _reference_ to the given label. See previous example.

- `MOVE` is used to **store** results(assignments). That's way it has two forms:
   * `MOVE(TEMP r, exp)` evaluates the expression and store the result in _register r_.
   * `MOVE(MEM exp1, exp2)` First evaluate _exp1_ yielding address `addr`(store location). Then evaluate _exp2_ and
     store the result into _wordSize_(defined in Frame interface) bytes of memory **starting** at `addr`.

- `EXP e` evaluate given expression and **discards** the result.
   In which cases could be used? `externalCall` for example if we call procedure.

- `TEMP e` assume it is a register.

#### Exp

- `MEM(e)` when is used as the **left child** of a `MOVE`, it means _"store"_, but anywhere else it means
  **"fetch"** - take the contents of _WORDSIZE_ bytes of memory from address `e`! In other words:
   Computes value of `e` and looks up contents of memory at that address. Analog of C pointers `MEM e -> *e`.

**Order of parameter execution is important!** See chapter 8 introduction.

- `CALL(exp, params)` first execute `exp` to infer function name then calls it with given parameters.

- `ESEQ(stm, exp)` first statement is evaluated for **side effects** (do not return value), then _exp_ is evaluated
   for a result. Evaluates an expression `exp` **after** completion of a statement `stm` that might affect result of `exp`.

### Translation example

Note: `t` and `f` are labels.

  - `x>y` becomes `Cx(fun (t, f) -> CJUMP(GT, x, y, t, f))`

  - `a>b | c<d` becomes `Cx(fun (t, f) -> SEQ( CJUMP(GT, a, b, t, z), SEQ(LABEL z, CJUMP(LT, c, d, t, f))))`

  - `a:=x>y` becomes `MOVE (TEMP(a), e)` so `MOVE(TEMP(a), unEx(Cx(t, f) -> ...)`

Tip: The _"wise"_ assignment of variables to caller/callee-save registers is an important
     compiler optimization (backed  up  by **data-flow analysis** techniques).

### Example

```
n = 0;
while (n < 10) {
   n = n + 1
}
```
becomes:

```
SEQ(
    MOVE(TEMP n, CONST 0),
  LABEL(HEAD),
    CJUMP(LT, TEMP n, CONST 10),
          BODY, END),

  LABEL body,
    MOVE(TEMP n, ADD(TEMP(n),
         CONST(1))),
    JUMP(NAME(HEAD)),
  LABEL(END)
)
```

### FOR loop translation

```
let var i := lo
    var limit := hi
in
    if lo <= hi
        then
            while 1 do
                ( body;
                  if i < limit
                      then i := i + 1
                      else break )
 end
```

### Function IR definition

Each Tiger function is translated into a _prologue_, a _body_  and an _epilogue_:

1. pseudo-instructions for the function beginning
2. a **label** definition for the function name
3. an instruction to adjust the **stack pointer**
4. instructions to **save** "escaping" arguments
5. **store** instructions to save any callee-save registers

6. the function body

7. an instruction to deliver the **function result**
8. load instructions to **restore** the callee-save registers
9. an instruction to reset the **stack pointer**
10. a jump to the **return address**
11. pseudo-instructions for the function end

For `procEntryExit1` steps _2, 4, 5, 6, 7_ and _8_ should be implemented.
Other depends from frame size and will be implemented when register are allocated.

### Frame

_Look at the created list frame as program that operates on given machine architecture._
_It contains instructions that should be executed or strings(do nothing)._

#### MIPS Simulator

https://thaumicmekanism.github.io/venus/

#### VSCode plugin for Tiger language

https://github.com/yunyu/tiger-vscode