## Tiger Lexer

Program form Lexer point of view:

```
Program ::= (Token | Whitespace | Comment)* EOF
```

```
<Token> ::= <Keywords>
         | <Punctuation>
         | <Operator>
         | <Integer-Literal>
         | <String-Literal>
         | <Identifier>
```

```
<Keywords> ::= array | break | do | else | end | for |
               function | if | in | let | nil | of |
               then | to | type | var | while
```

```
<Punctuation> ::= := | . | ( | ) | [ | ] | { | } | ; | : | ,
```

```
<Operator> ::= & | | | * | / | + | - | = | <> | > | >= | < | <=
```

An integer literal is a sequence of one or more digits form `0-9`

```
<Integer-Literal> ::= <Digit> <Digit>*
```

A string literal is a sequence, between quotes `"`, of zero or more printable characters,
spaces, or escape sequences. Each escape sequence is introduced by the escape character `\`,
and stands for a character sequence.

```
<String-Literal> ::= " (ASCII printable character | <Escape>)* "
```

```
<Escape> ::= \n | \t | \r | \\ | \" | \^a | \^b | \^v | \^f |
             \ddd | \s...s\
```

**NOTE:** `\s..s..s\` ignores spaces or newlines between `\` (`s` is spaces or newlines)

An identifier starts with a letter, followed by zero or more letters, underscores, or
digits. Keywords cannot be used as identifiers.<br/> **Identifiers are case-sensitive.**

```
<Identifier> ::= <Letter> (<Letter> | _ | <Digit>)*
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

A comment starts with `/*` and ends with `*/`. **Comments could be nested.**
