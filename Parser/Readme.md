## Tiger Parser

### EBNF

Tip: Be careful with <...>* definitions when translate specification to parser definition!
     As you know <Dec>+ is <Dec> [<Dec>]*.

#### Program

Tiger programs do not have arguments: a program is just an expression.

```
<Program> ::= <Exp> EOF

<Exp>     ::= <IfThenElse> | <WhileExp> | <ForExp> | <LetExp> | <SeqExp> |
              <Assign> | <LValue> | <FunCall> | <InfixExp> | <Negation> |
              <RecCreate> | <ArrCreate> | INT | STRING | nil | break
```

#### Expression

```
<IfThenElse> ::=
              | if <Exp> then <Exp>
			  | if <Exp> then <Exp> else <Exp>

<WhileExp>   ::= while <Exp> do <Exp>

<ForExp> ::=
          | for ID := <Exp> to <Exp> do <Exp>

<LetExp> ::= let <DecSeq> in <ExpSeq> end

<SeqExp> ::= ( TwoOrMoreExp )

<DecSeq> ::=
          | <empty>
		  | <Dec> <DecSeq>

<ExpSeq> ::=
          | <empty>
		  | <Exp>
		  | <Exp> ; ExpSeq

<TwoOrMoreExp> ::=
                | <Exp> ; <Exp>
				| <Exp> ; <Exp> ; ExpSeq
```

#### Declaration

A declaration-sequence is a sequence of type, value, and function declarations; no
punctuation separates or terminates individual declarations.

```
<Dec> ::=
       | <TyDec>
       | <VarDec>
       | <FunDec>

<TyDec>  ::=
          | type TYID = <Ty>

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
<Ty> ::=
      | TYID
      | { <FieldDec> }
      | array of ID

<FieldDec> ::=
            | <empty>
            | ID : TYID
			| ID : TYID , FieldDec
```

#### Variables and expressions

```
<Assign> ::=
          | <LValue> := <Exp>

<LValue> ::=
          | ID
          | <LValue> . ID
          | <LValue> [ <Exp> ]

<FunCall>  ::= ID ( ExpSeq )

<InfixExp> ::= <Exp> <Op> <Exp>

<Negation> ::= - <Exp>
```

#### Creation

```
<RecCreate> ::= TYID { RecFieldSeq }
<ArrCreate> ::= TYID [ <Exp> ] of <Exp>

RecFieldSeq ::=
             | <empty>
			 | ID = <Exp>
			 | ID = <Exp> , RecFieldSeq
```

#### Terminals

```
<Op> ::= + | - | * | / |
         = | <> | > | < | >= | <=
         & | |
```

### Precedence and associativity

Tip: This is only preliminary. Analyze shift/reduce errors from FsYaccLex.

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
