module Tree

// Intermidate Language is choosen to be - simple expression tree

type Label = Temp.Label

type Size = int

type Stm =
    | SEQ of Stm * Stm
    | LABEL of Label
    | JUMP of Exp * Label list
    | CJUMP of RelOp * Exp * Exp * Label * Label
    | MOVE of Exp * Exp
    | EXP of Exp

and Exp = 
    | BINOP of BinOp * Exp * Exp
    | MEM of Exp
    | TEMP of Temp.Temp
    | ESEQ of Stm * Exp
    | NAME of Label
    | CONST of int
    | CALL of Exp * Exp list
    
and BinOp = 
    | PLUS | MINUS
    | MUL  | DIV 
    | AND  | OR 
    | LSHIFT | RSHIFT | ARSHIFT 
    | XOR

and RelOp = 
    | EQ  | NE 
    | LT  | GT 
    | LE  | GE 
    | ULT | ULE 
    | UGT | UGE