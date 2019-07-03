module Codegen

// NOTE: If open Assem `Tree.MOVE` clash with `Assem.MOVE`

open Tree

// Should not overlap
let calldefs = Frame.RV :: Frame.RA :: Frame.callerSavesRegs @ Frame.argRegs

// Accept canonical tree
let codegen (frame: Frame.Frame) stm =

    let alist = ref []

    let emit instr =
        alist := instr :: !alist

    let result func = let t = Temp.newTemp()
                      func t  // apply
                      t

    let rec munch_args i = function
        | []            -> []

        | arg :: args when i < List.length Frame.argRegs
                        -> let temp = munch_exp arg
                           let argreg = List.item i Frame.argRegs

                           emit (Assem.MOVE { assem = "move 'd0, 's0";
                                        src = temp;
                                        dst = argreg })

                           argreg :: (munch_args (i + 1) args)

        | args          -> List.mapi (fun i arg -> let temp = munch_exp arg
                                                   emit (Assem.OPER { assem = (sprintf "sw 's0, %i('s1)" (Frame.WORDSIZE * i));
                                                                src = [temp; Frame.SP]; dst = []; jump = None });
                                                                temp) args

    and munch_stm stm =
        match stm with
        | SEQ (a, b)    -> munch_stm a; munch_stm b

        | MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2)
                        -> emit (Assem.OPER { assem = (sprintf "sw 's0, %i('s1)" i);
                                        src = [munch_exp e2; munch_exp e1]; dst = []; jump = None})

        | MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2)
                        -> emit (Assem.OPER { assem = (sprintf "sw 's0, %i('s1)" i);
                                        src = [munch_exp e2; munch_exp e1]; dst = []; jump = None })

        | MOVE (MEM (CONST i), e2)
                        -> emit (Assem.OPER { assem = (sprintf "sw 's0, %i" i);
                                        src = [munch_exp e2]; dst = []; jump = None })

        | MOVE (MEM (e1), e2)
                        -> emit (Assem.OPER { assem = "sw 's0, ('s1)";
                                        src = [munch_exp e2; munch_exp e1]; dst = []; jump = None })

        | LABEL lab     -> emit (Assem.LABEL { assem = (sprintf "%s:" (Temp.stringOfLabel lab)); lab = lab })

        | EXP (CALL (NAME lab, args))
                        -> emit (Assem.OPER { assem = (sprintf "jal %s" (Temp.stringOfLabel lab));
                                        src = munch_args 0 args;
                                        dst = calldefs;
                                        jump = None })

        | MOVE (TEMP t, CALL (NAME lab, args))
                        -> emit (Assem.OPER { assem = (sprintf "jal %s" (Temp.stringOfLabel lab));
                                        src = munch_args 0 args;
                                        dst = calldefs;
                                        jump = None });

                           emit (Assem.MOVE { assem = "move 'd0, 's0";
                                        src = Frame.RV;
                                        dst = t })

        | MOVE (TEMP t, e1)
                        -> emit (Assem.MOVE { assem = "move 'd0, 's0";
                                        src = munch_exp e1;
                                        dst = t })

        | JUMP (NAME lab, labs) when labs = [lab]
                        -> emit (Assem.OPER { assem = "j 'j0";
                                        src = []; dst = [];
                                        jump = Some labs })

        | JUMP (e, labs)
                        -> emit (Assem.OPER { assem = "jr 's0";
                                        src = [munch_exp e]; dst = [];
                                        jump = Some labs})

        | CJUMP (relop, e1, e2, trueLab, falseLab)
                        -> let op2assem = function
                               | EQ -> "beq"
                               | NE -> "bne"
                               | LT -> "blt"
                               | GT -> "bgt"
                               | LE -> "ble"
                               | GE -> "bge"
                               | ULT -> "bltu"
                               | ULE -> "bleu"
                               | UGT -> "bgtu"
                               | UGE -> "bgeu"

                           emit (Assem.OPER { assem = (sprintf "%s 's0, 's1, 'j0" (op2assem relop));
                                        src = [munch_exp e1; munch_exp e2];
                                        dst = [];
                                        jump = Some [trueLab; falseLab] })

        | MOVE (BINOP (op, _, _), _)
                        -> failwithf "ERROR: `%A` should not be used." op
        | MOVE (exp, _)
                        -> failwithf "ERROR: `%A` should be eliminated by canonical algorithm." exp
        | EXP (_)       -> failwithf "ERROR: Maximal Munch algorithm implementation is wrong."

    and munch_exp exp =
        match exp with
        | MEM (BINOP (PLUS, e1, CONST i))
                        -> result (fun r -> emit (Assem.OPER { assem = (sprintf "lw 'd0, %i('s0)" i);
                                                         src = [munch_exp e1]; dst = [r]; jump = None }) )

        | MEM (BINOP (PLUS, CONST i, e1))
                        -> result (fun r -> emit (Assem.OPER { assem = (sprintf "lw 'd0, %i('s0)" i);
                                                         src = [munch_exp e1]; dst = [r]; jump = None }) )

        | MEM (CONST i)
                        -> result (fun r -> emit (Assem.OPER { assem = (sprintf "lw 'd0, %i" i);
                                                         src =[]; dst = [r]; jump = None }))

        | MEM (e1)      -> result (fun r -> emit (Assem.OPER { assem = "lw 'd0, ('s0)";
                                                         src = [munch_exp e1]; dst = [r]; jump = None }) )

        | BINOP (PLUS, e1, CONST i)
                        -> result (fun r -> emit (Assem.OPER { assem = (sprintf "addi 'd0, 's0, %i" i);
                                                           src = [munch_exp e1]; dst = [r]; jump = None }) )

        | BINOP (PLUS, CONST i, e1)
                        -> result (fun r -> emit (Assem.OPER { assem = (sprintf "addi 'd0, 's0, %i" i);
                                                         src = [munch_exp e1]; dst = [r]; jump = None }) )

        | BINOP (PLUS, e1, e2)
                        -> result (fun r -> emit (Assem.OPER { assem = "add 'd0, 's0, 's1";
                                                         src = [munch_exp e1; munch_exp e2]; dst = [r];
                                                         jump = None }) )

        | BINOP (MINUS, e1, e2)
                        -> result (fun r -> emit (Assem.OPER { assem = "sub 'd0, 's0, 's1";
                                                         src = [munch_exp e1; munch_exp e2]; dst = [r];
                                                         jump = None }) )

        | BINOP (MUL, e1, e2)
                        -> result (fun r -> emit (Assem.OPER { assem = "mulo 'd0, 's0, 's1";
                                                        src = [munch_exp e1; munch_exp e2]; dst = [r];
                                                        jump = None }) )

        | BINOP (DIV, e1, e2)
                        -> result (fun r -> emit (Assem.OPER { assem = "div 'd0, 's0, 's1";
                                                         src = [munch_exp e1; munch_exp e2]; dst = [r];
                                                         jump = None }) )

        | CONST i       -> result (fun r -> emit (Assem.OPER { assem = (sprintf "li 'd0, %i" i);
                                                         src = []; dst = [r]; jump = None }) )

        | NAME lab      -> result (fun r -> emit (Assem.OPER { assem = (sprintf "la 'd0, %s" (Temp.stringOfLabel lab));
                                                         src = []; dst = [r]; jump = None }) )

        | TEMP t        -> t

        // Errors zone

        | BINOP (AND, _, _) | BINOP (OR, _, _)
                        -> failwithf "ERROR: Logical binop should have already been translated into if-exp in the parser."

        | BINOP (LSHIFT, _, _) | BINOP (RSHIFT, _, _) | BINOP (ARSHIFT, _, _) | BINOP (XOR, _, _)
                        -> failwithf "ERROR: Shifts and `XOR` should not be used."

        | ESEQ _        -> failwithf "ERROR: `ESEQ` should be eliminated by canonical algorithm."
        | CALL _        -> failwithf "ERROR: `CALL` should be eliminated by canonical algorithm."

    munch_stm stm

    // Algorithm works top-down so revers the result
    List.rev !alist