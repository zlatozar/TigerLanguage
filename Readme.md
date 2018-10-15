## Tiger Language in F#

This project contains Tiger language implementation described in book <br/>
_"Modern Compiler Implementation in ML" (1998) by Andrew Appel_

The main ideas behind every step during development are:

- control the constraints and give feedback on error (lexer, parser and syntactic analysis)
- linearization of syntax tree and represent it in a sequential form (intermediate representation)
- efficiency/optimization (the whole compiler backend)
