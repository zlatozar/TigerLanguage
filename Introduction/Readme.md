## Straight-line programming language

### Lexer

Program from Lexer point of view

```
Program ::= (Token | Blank)* EOF
```

```
<Token> ::= <Integer-Literal> | <Identifier> | <Operator> |
            print | ( | ) | , | ; | :=
```

```
<Integer-Literal> ::= <Digit> <Digit>*
```

```
<Identifier> ::= <Letter> (<Letter> | <Digit>)*
```

```
<Blank> ::= space | tab | EOF
```

```
<Letter> ::= A | B | C | D | E | F | G | H | I | J | K | L | M |
             N | O | P | Q | R | S | T | U | V | W | X | Y | Z |
             a | b | c | d | e | f | g | h | i | j | k | l | m |
             n | o | p | q | r | s | t | u | v | w | x | y | z
```

```
<Digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
```

```
<Operator> ::= + | - | * | /
```

### Parser

```
Stmt ::=
      | Stmt `;` Stmt             // Compound Statement

Stmt ::= ID `:=` Exp              // Assign Statement

Stmt ::= `print` `(` ExpList `)`  // Print Statement

// one or many expressions (list of expressions)

ExpList ::= Exp                   // Last Expression List
ExpList ::= Exp `,` ExpList       // Pair Expression List

Exp  ::= ID                       // Identifier

Exp  ::= NUM                      // Integer Literal

Exp  ::= Exp BinOp Exp            // Operation Expression

Exp  ::= `(` Stmt `,` Exp `)`     // Sequence Expression

BinOp   ::= `+`                   // Plus
BinOp   ::= `-`                   // Minus
BinOp   ::= `*`                   // Times
BinOp   ::= `/`                   // Divide
```

### Example

```
a := 5 + 3 ; b := ( print ( a , a - 1 ) , 10 * a ) ; print ( b )
```
