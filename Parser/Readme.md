## Tiger Parser

### EBNF


#### Program

Tiger programs do not have arguments: a program is just an expression.

```
<Program> ::= <Exp>

<Exp>     ::= <IfThenElse> | <WhileExp> | <ForExp> | <LetExp> | <SeqExp> |
              <Assign> | <LValue> | <FunCall> | <InfixExp> | <Negation> |
			  <RecCreate> | <ArrCreate> | INT | STRING | nil | break
```

#### Expression

```
<IfThenElse> ::= if <Exp> then <Exp> [else <Exp>]

<WhileExp> ::= while <Exp> do <Exp>

<ForExp> ::= for ID := <Exp> to <Exp> do <Exp>

<LetExp> ::= let <Dec>+ in <Exp> [; <Exp>]* end

<SeqExp> ::= ( <Exp> [; <Exp>]* )

```

#### Declaration

A declaration-sequence is a sequence of type, value, and function declarations; no
punctuation separates or terminates individual declarations.

```
<Dec> ::= <TyDec>
       | <VarDec>
	   | <FunDec>

<TyDec> ::= type TYID = <Ty>

<VarDec> ::= var ID := <Exp>
          | var ID : TYID := <Exp>

<FunDec> ::= function ID ( <FieldDec> ) = <Exp>        // procedure, do not return
          | function ID ( <FieldDec> ) : TYID = <Exp>  // function
```

#### Data types

```
<Ty> ::= TYID
	  | { <FieldDec> }
      | array of ID

<FieldDec> ::= <empty>
            | ID : TYID [, ID : TYID]*
```

#### Variables and expressions

```
<Assign> ::= <LValue> := <Exp>

<LValue> ::= ID
          | <LValue> . ID
		  | <LValue> [ <Exp> ]

<FunCall> ::= ID ( <Exp> [, <Exp>] )

<InfixExp> ::= <Exp> OP <exp>

<Negation> ::= - <Exp>
```

#### Creation

```
<RecCreate> ::= TYID { ID = <Exp> [, id = <Exp>] }
<ArrCreate> ::= TYID [ <Exp> ] of <Exp>
```

#### Terminals

```
OP ::= + | - | * | / |
       = | <> | > | < | >= | <=
	   & | |
```

### Precedence and associativity

All operators in Tiger are **left associative**, except for the _comparison operators_,
which do no associate.

```
()
parentheses
-                  negation
* /                multiply, divide
+ -                add, subtract
= < > <> >= <=     comparison
&                  logical and
|                  logical or
:=                 assignment

```
