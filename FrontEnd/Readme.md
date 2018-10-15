## Intermediate code generation

Includes _Chapter 6_ and _Chapter 7_

Before writing a code generator, however, we must decide how to marshal the resources of
the target machine (instructions, storage, and system software) in order to implement the
source language. This is called **run-time organization**, and is the subject of this two
chapters.

The following are key issues in run-time organization:

- Data representation: How should we represent the values of each source-language
type in the target machine?

- Expression evaluation: How should we organize the evaluation of expressions,
taking care of intermediate results?

- Storage allocation: How should we organize storage for variables, taking into
account the different lifetimes of global, local, and heap variables?

- Routines: How should we implement procedures, functions, and parameters, in terms
of low-level routines?

In order to make rational programming decisions, the application programmer should have a
feel for the efficiency of various high-level language constructs. An example is the
choice of data structures: records and static arrays can be represented very efficiently,
but the representations of dynamic arrays and recursive types carry overheads
(indirect addressing, garbage collection) that might be unacceptable in some applications.

### Data representation

#### Principles

Programming languages provide high-level data types such as truth values, integers,
characters, records, and arrays, together with operations over these types. Target
machines provide only machine 'types' such as bits, bytes, words, and double-words,
together with low-level arithmetic and logical operations. To _bridge_ the semantic gap
between the source language and the target machine, the implementor must decide how
to represent the source language's types and operations in terms of the target machine's
types and operations.

We should bear in mind the following _fundamental_ principles of data representation:

- **Nonconfusion**: Different values of a given type should have different representations.
- **Uniqueness**: Each value should always have the same representation.

and _pragmatic_ issues in data representation:

- **Constant-size representation**: The representations of all values of a given type should
occupy the same amount of space.
- **Direct representation** or **indirect representation**: Should the values of a given type be
represented directly, or indirectly through pointers?

The choice of direct or indirect representation is a key design decision in run-time
organization. Indirect representation is essential for types whose values vary greatly in
size.

#### Records

  A record consists of several fields, each of which has an identifier. A record type
designates the identifiers and types of its fields, and all the records of a particular type
have fields with the same identifiers and types. The fundamental operation on records is
field selection, whereby we use one of the field identifiers to access the corresponding
field.
  There is an obvious and good direct representation for records: we simply juxtapose
the fields, i.e., make them occupy consecutive positions in storage. This representation
is compact, and makes it easy to implement field selection very efficiently.

#### Expression evaluation

The implementation problem is the need to keep intermediate results somewhere, during
evaluation of the more complicated expressions. Many machines provide registers that can
be used to store intermediate results.

### Routine protocol

  When a routine is called, the arguments are computed by the caller, and used by the called
routine (callee). Thus we need a suitable routine protocol, a convention to ensure that
the caller deposits the arguments in the place where the called routine expects to find
them.  Conversely, the routine's result (if any) is computed by the routine, and used by
the caller. Thus the routine protocol must also ensure that, on return, the called routine
deposits its result in the place where the caller expects to find it.

  According to the routine protocol, the arguments to be passed to a routine are deposited
at the top of the caller's frame (or at the top of the globals, if the caller is the main
program). Since the latter frame is just under the called routine's frame, the called
routine can find its arguments just under its own frame. In other words, the arguments
have small negative addresses relative to the base of the called routine's frame. In all
other respects, they can be accessed just like variables local to the called routine.

#### View Shift

The difference in the caller's method of access and the callee's is called the _view shift_.

The frame module provides a sequence of instructions, the view shift, ensuring that,
on function entry, parameters are moved into the callee's frame where they are expected by
the callee's body. See 'instrs' field in Frame.