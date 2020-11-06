## Tiger Language Specification

Tip: Use the [Ohm Editor](https://ohmlang.github.io/) while developing your grammar.

You can write all your tests in the Ohm editor and the tests run as you type! Go
ahead and develop test cases in the Ohm editor as you develop your grammar,
then, when you are satisfied, you can move them into your project.

Now, since Ohm does most of the work, the syntax checker is pretty
straightforward. It will be a function that takes in a string representation of
a candidate Tiger program, and returns whether or not the program is
syntactically correct. Next step is to go through the Ohm grammar and ask yourself
which things are relevant to the abstract syntax and which are not. In other words
- what information is needed expression to be translated/executed.

```
Tiger {
  Program      = Exp
  Exp          = let Dec+ in Exps endkw                  -- let
               | if Exp then Exp (else Exp)?             -- if
               | while Exp do Exp                        -- while
               | for id ":=" Exp to Exp do Exp           -- for
               | Lvalue ":=" Exp                         -- assign
               | break                                   -- break
               | Exp1

  Dec          = TypeDec | VarDec | FunDec
  TypeDec      = type typeId "=" Type
  Type         = typeId                                  -- named
               | ArrayType
               | RecordType
  ArrayType    = array of typeId
  RecordType   = "{" ListOf<Field, ","> "}"
  FunDec       = function id Params (":" typeId)? "=" Exp
  VarDec       = var id (":" typeId)? ":=" Exp
  Field        = id ":" typeId
  Params       = "(" ListOf<Param, ","> ")"
  Param        = id ":" typeId

  Exps         = ListOf<Exp, ";">
  Exp1         = Exp1 "|" Exp2                           -- binary
               | Exp2
  Exp2         = Exp2 "&" Exp3                           -- binary
               | Exp3
  Exp3         = Exp4 relop Exp4                         -- binary
               | Exp4
  Exp4         = Exp4 addop Exp5                         -- binary
               | Exp5
  Exp5         = Exp5 mulop Exp6                         -- binary
               | Exp6
  Exp6         = "-" Exp7                                -- negation
               | Exp7
  Exp7         = Call | ArrayExp | RecordExp | ExpSeq | Literal | Lvalue

  Literal      = intlit
               | stringlit
               | nil                                     -- nil
  Lvalue       = Lvalue "[" Exp "]"                      -- subscripted
               | Lvalue "." id                           -- field
               | id                                      -- id
  ArrayExp     = typeId "[" Exp "]" of Exp
  RecordExp    = typeId "{" ListOf<Binding, ","> "}"
  Binding      = id "=" Exp
  Call         = id "(" ListOf<Exp, ","> ")"
  ExpSeq       = "(" ListOf<Exp, ";"> ")"

  array        = "array" ~idchar
  break        = "break" ~idchar
  do           = "do" ~idchar
  else         = "else" ~idchar
  endkw        = "end" ~idchar
  for          = "for" ~idchar
  function     = "function" ~idchar
  if           = "if" ~idchar
  in           = "in" ~idchar
  let          = "let" ~idchar
  nil          = "nil" ~idchar
  of           = "of" ~idchar
  then         = "then" ~idchar
  to           = "to" ~idchar
  type         = "type" ~idchar
  var          = "var" ~idchar
  while        = "while" ~idchar
  keyword      = array | break | do | else | for | function | if
               | in | let | nil | of | then | to | type | var | while

  id           = ~keyword letter idchar*
  idchar       = letter | digit | "_"
  typeId       = id

  intlit       = digit+
  stringlit    = "\"" char* "\""
  char         = ~"\\" ~"\"" ~"\n" any | escape
  escape       = "\\" ("\\" | "\"" | "n" | "t" | codepoint)
  codepoint    = "u{" hexDigit+ "}"                      -- changed
  mulop        = "*" | "/"
  addop        = "+" | "-"
  relop        = "<=" | "<>" | "<" | ">=" | ">" | "="
  space       := " " | "\t" | "\n" | comment
  comment      = "/*" (~"*/" any)* "*/"
}
```
