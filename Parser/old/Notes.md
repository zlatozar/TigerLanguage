## How to deal with shift/reduce conflicts

**Existing**

- `state 20` on terminal LBRACK between {[explicit right 9999] shift(60)} and {noprec reduce(LValue:'ID')}

```
immediate action:   reduce Exp --> LetExp  gotos:state 20:  items:    LValue -> 'ID' .
    FunCall -> 'ID' . 'LPAREN' ActualParams 'RPAREN'
    RecCreate -> 'ID' . 'LBRACE' RecAggregate 'RBRACE'
    ArrCreate -> 'ID' . 'LBRACK' Exp 'RBRACK' 'OF' Exp
  actions:    action 'EOF' (noprec):   reduce LValue --> 'ID'
    action 'EQ' (noprec):   reduce LValue --> 'ID'
```

Is it `ID` or `ID` is a part of something "bigger"? So **shifting** will not help for:
`arr1[2]` - **reduce** should be used. `LValue` can't be solved without looking 2 symbols
ahead but that is impossible. So it should be transformed.

- `state 28` on terminal EQ between {[explicit nonassoc 9997] shift(138)} and {noprec reduce(InfixOp:Exp Op Exp)}

```
state 28:  items:    InfixOp -> Exp . Op Exp
    InfixOp -> Exp Op Exp .
  actions:    action 'EOF' (noprec):   reduce InfixOp --> Exp Op Exp
    action 'EQ' (explicit nonassoc 9997):   shift 138
	....
```

`%nonassoc` forbid to have sequence of mathematical operations at the same level. Not harmful.

**Solved**

- `state 33` on terminal EQ between {[explicit nonassoc 9997] shift(138)} and {noprec reduce(ArrCreate:'ID' 'LBRACK' Exp 'RBRACK' 'OF' Exp)}

```
state 33:  items:    InfixOp -> Exp . Op Exp
    ArrCreate -> 'ID' 'LBRACK' Exp 'RBRACK' 'OF' Exp .
  actions:    action 'EOF' (noprec):   reduce ArrCreate --> 'ID' 'LBRACK' Exp 'RBRACK' 'OF' Exp
    action 'EQ' (explicit nonassoc 9997):   shift 138
    action 'NEQ' (explicit nonassoc 9997):   shift 139
    ...
```

Solved by adding `%left OF`

- `state 41` on terminal EQ between {[explicit nonassoc 9997] shift(138)} and {noprec reduce(IfExp:'IF' Exp 'THEN' Exp)}

```
state 41:  items:    InfixOp -> Exp . Op Exp
    IfExp -> 'IF' Exp 'THEN' Exp .
    IfExp -> 'IF' Exp 'THEN' Exp . 'ELSE' Exp
  actions:    action 'EOF' (noprec):   reduce IfExp --> 'IF' Exp 'THEN' Exp
    action 'EQ' (explicit nonassoc 9997):   shift 138
    action 'NEQ' (explicit nonassoc 9997):   shift 139
    ...
```

Not harmful but solved by adding `%left THEN` with lest priority than `%left ELSE`.

- `state 42` on terminal EQ between {[explicit nonassoc 9997] shift(138)} and {noprec reduce(IfExp:'IF' Exp 'THEN' Exp 'ELSE' Exp)}

```
state 42:  items:    InfixOp -> Exp . Op Exp
    IfExp -> 'IF' Exp 'THEN' Exp 'ELSE' Exp .
  actions:    action 'EOF' (noprec):   reduce IfExp --> 'IF' Exp 'THEN' Exp 'ELSE' Exp
    action 'EQ' (explicit nonassoc 9997):   shift 138
    ...
```

- `state 44` on terminal EQ between {[explicit nonassoc 9997] shift(138)} and {noprec reduce(WhileExp:'WHILE' Exp 'DO' Exp)}

```
state 42:  items:    InfixOp -> Exp . Op Exp
    IfExp -> 'IF' Exp 'THEN' Exp 'ELSE' Exp .
  actions:    action 'EOF' (noprec):   reduce IfExp --> 'IF' Exp 'THEN' Exp 'ELSE' Exp
    action 'EQ' (explicit nonassoc 9997):   shift 138
	...
```

- `state 47` on terminal EQ between {[explicit nonassoc 9997] shift(138)} and {noprec reduce(ForExp:'FOR' 'ID' 'ASSIGN' Exp 'TO' Exp 'DO' Exp)}

```
state 47:  items:    InfixOp -> Exp . Op Exp
    ForExp -> 'FOR' 'ID' 'ASSIGN' Exp 'TO' Exp 'DO' Exp .
  actions:    action 'EOF' (noprec):   reduce ForExp --> 'FOR' 'ID' 'ASSIGN' Exp 'TO' Exp 'DO' Exp
    action 'EQ' (explicit nonassoc 9997):   shift 138
	...
```
Solved by adding `%left DO`
