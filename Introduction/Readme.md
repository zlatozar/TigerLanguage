## Straight-line programming language

### Lexer

```
Program ::= (Token | Blank)*
```

\<Token\> **::=** \<Integer-Literal\> | \<Identifier\> | \<Operator\> | <br/>
&nbsp;&nbsp;&nbsp;&nbsp;`print` | `(` | `)` | `,` | `;` | `:=`<br/>

\<Integer-Literal\> **::=** \<Digit\> \<Digit\>*

\<Identifier\>      **::=** \<Letter\> (\<Letter\> | \<Digit\>)*

\<Blank\>           **::=** space | tab | EOF

\<Letter\>          **::=**   `A` | `B` | `C` | `D` | `E` | `F` | `G` | `H` | `I` | `J` | `K` | `L` | `M` | <br/>
&nbsp;&nbsp;&nbsp;&nbsp;`N` | `O` | `P` | `Q` | `R` | `S` | `T` | `U` | `V` | `W` | `X` | `Y` | `Z` | <br/>
&nbsp;&nbsp;&nbsp;&nbsp;`a` | `b` | `c` | `d` | `e` | `f` | `g` | `h` | `i` | `j` | `k` | `l` | `m` | <br/>
&nbsp;&nbsp;&nbsp;&nbsp;`n` | `o` | `p` | `q` | `r` | `s` | `t` | `u` | `v` | `w` | `x` | `y` | `z` <br/>

\<Digit\>        **::=**  `0` | `1` | `2` | `3` | `4` | `5` | `6` | `7` | `8` | `9` <br/>
\<Operator\>     **::=**  `+` | `-` | `x` | `/`


### Parser

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
