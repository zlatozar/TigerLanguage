## Tiger Parser

Use to use language specification. If you create EBNF half of the work is done.

### EBNF

-  Be careful with <...>* definitions when translate specification to parser definition!
   As you know <Dec>+ is <Dec> [<Dec>]*.

- `if..then` and `if..then..else` is the signal of **dangling else** problem.

- When use parser generator `left recursion` shouldn't be eliminated.

- Remember that the output of the lexer is the input for the parser so careful study the
  lexer tokens. `STRING`, `INT`, `NIL` and `BREAK`.

#### Program

Tiger programs do not have arguments: a program is just an expression.

```
<Program> ::= <Exp> EOF

<Exp>     ::= STRING | INT | NIL | BREAK | <LValue> | <Negation> |
              <InfixOp> | <Assign> | <FunCall> | <RecCreate> |
			  <ArrCreate> | <SeqExp> | <IfExp> | <WhileExp> |
			  <ForExp> | <LetExp>

```

#### Variables and Expressions

```
<LValue> ::=
          | ID
          | <LValue> . ID
          | <LValue> [ <Exp> ]

<Negation> ::= - <Exp>

<InfixOp> ::= <Exp> <Op> <Exp>

<Assign> ::=
          | <LValue> := <Exp>

<FunCall> ::= ID ( <ActualParams> )

<ActualParams> ::=
                | empty
		        | <ExpList>

<ExpList> ::=
           | <Exp>
		   | <ExpList> , <Exp>
```

#### Record and Array Literals

```
<RecCreate> ::= TYID { <RecAggregate> }

<ArrCreate> ::= TYID [ <Exp> ] of <Exp>

<RecAggregate> ::=
                | <empty>
			    | <FieldList>

<FieldList> ::=
             | ID := <Exp>
			 | <FieldList> , ID := <Exp>
```


#### Expressions

```
<SeqExp> ::= ( <TwoOrMoreExp> )

<TwoOrMoreExp> ::=
                | <Exp> ; <Exp>
				| <TwoOrMoreExp> ; <ExpSeq>

<ExpSeq> ::=
          | <Exp>
		  | <ExpSeq> ; <Exp>

<IfExp> ::=
         | if <Exp> then <Exp>
		 | if <Exp> then <Exp> else <Exp>

<WhileExp> ::= while <Exp> do <Exp>

<ForExp> ::=
          | for ID := <Exp> to <Exp> do <Exp>

<LetExp> ::= let <Declarations> in <Stmts> end

<Declarations> ::=
                | <empty>
				| <DecList>

<DecList> ::=
           | <Dec>
		   | <DecList> <Dec>

<Stmts> ::=
         | <empty>
		 | <ExpSeq>
```

#### Declaration

A declaration-sequence is a sequence of type, value, and function declarations; no
punctuation separates or terminates individual declarations.

```
<Dec> ::=
       | <TypeDec>
       | <VarDec>
       | <FunDec>

<TypeDec>  ::=
            | type TYID = <Type>

<VarDec> ::=
          | var ID := <Exp>
          | var ID : TYID := <Exp>

<FunDec> ::=
          | function ID ( <FieldDec> ) = <Exp>         // procedure, do not return
          | function ID ( <FieldDec> ) : TYID = <Exp>  // function
```

Note: TYID is an identifier defined by a type declaration. Lexer could recognize only
      IDs (identifiers) so in parser definition use ID instead of TYID.

#### Data types

```
<Type> ::=
        | TYID
        | { <FieldDec> }
        | array of TYID

<FieldDec> ::=
            | <empty>
            | <TypeFieldList>

<TypeFieldList> ::=
                 | ID : TYID
			     | <TypeFieldList> , ID : TYID

```
#### Terminals

Tip: This definition could be inlined.

```
<Op> ::= + | - | * | / |
         = | <> | > | < | >= | <=
         & | |
```

### Precedence and Associativity

Tip: This is only preliminary. Analyze **shift/reduce** errors from `FsYaccLex`.

Here is the order, from lower to highest:

```
:=                 assignment (right)
|                  logical OR (left)
&                  logical AND (left)
= < > <> >= <=     comparison (do not associate)
+ -                add, subtract (left)
* /                multiply, divide (left)
-                  Negation (do not associate)
(                  LPAREN (do not associate)
```
