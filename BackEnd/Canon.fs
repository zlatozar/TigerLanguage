module Canon

open Tree

// Olin Shivers' version

// From an arbitrary Tree statement, produce a list of cleaned trees
// satisfying the following properties:
//    1. No SEQ's or ESEQ's
//    2. The parent of every CALL is an EXP(..) or a MOVE(TEMP t, ..)
//
// A list of these things looks much more like "generic" assembler:
//
//    FACT:				            // LABEL
//      if n=0 then L1 else L2		// CJUMP
//    L1:				            // LABEL
//      rv := 1				        // MOVE
//      goto DONE		            // JUMP
//    L2:                           // LABEL
//      r := FACT(n-1)	            // MOVE
//      a := n * r	                // MOVE
//      rv := a		                // MOVE
//      goto DONE	                // JUMP
//
// The right-hand sides of register assignments and memory stores can be pure
// expression trees or function calls. Funcall args must be pure expression trees.
let linearize stm0 =

    let nop = EXP (CONST 0)

    // Eliminate useless NOP or unite in a SEQ
    let ( % ) x y =
        match x, y with
        | EXP (CONST _), _ -> y
        | _, EXP (CONST _) -> x
        | _                -> SEQ (x, y)

    // Apply given function starting from the last expression
    let rec stmtSub f = function
        | SEQ (a, b) -> stmtSub f a && stmtSub f b
        | s          -> f s

    // 'stmt' - a list of Tree statements.
    // 'e'    - side-effect-free Tree expression -- won't write a temp or
    //          memory and guaranteed to terminate.
    //
    // - Return 'true' if we are sure that executing (stmt; exp) produces
    //   the same value as (t := exp; stmt; t) for some fresh variable 't'.
    // - Return 'false' if the two executions would differ... or if we're not sure.
    //
    // Control effects: Note that if 'stmt' contains a jump off to somewhere else,
    // neither expression produces any value at all! So they would commute. On
    // the other hand, if 'stmt' contains a jump outside of 'stmt', where we might
    // execute arbitrary code, *then jump back*, said arbitrary code might write
    // a location referenced by 'e'... so we throw up our hands and safely say 'false'.
    let rec commutes (stmt: Stm) (e: Exp) :bool =
        match stmt, e with
        | _, NAME _    -> true
        | _, CONST _   -> true
        | stmt, BINOP (_, e1, e2) -> commutes stmt e1 && commutes stmt e2
        | stmt, TEMP t            -> stmtSub (function
                                                | JUMP _            -> false
                                                | CJUMP _           -> false
                                                | EXP (CALL _)      -> true
                                                | MOVE (TEMP t', _) -> not (t = t')
                                                | MOVE _            -> true
                                                | EXP _             -> true
                                                | _                 -> failwithf "ERROR: Unexpected expression in `commute`."
                                             ) stmt

        | stmt, MEM _             -> stmtSub (function
                                                | MOVE (MEM _, _)  -> false
                                                | JUMP _           -> false
                                                | CJUMP _          -> false
                                                | EXP (CALL _)     -> false
                                                | MOVE (_, CALL _) -> false
                                                | _                -> true
                                             ) stmt

        | _ -> failwithf "ERROR: Unexpected statement in `commute`."

    // Boil out the side-effects into a Stm list, and render the left-over,
    // pure expressions. (stmt; exp) to (t := exp; stmt; t) if commute.
    let rec doExps = function
        | [] -> (nop, [])
        | h::t -> let (stm1', e') = doExp h
                  let (stm2', el) = doExps t

                  if (commutes stm2' e') then
                      (stm1' % stm2', e' :: el)
                  else
                      let t = TEMP (Temp.newTemp())
                      (stm1' % MOVE (t, e') % stm2', t :: el)

    // Turn a compex statement into a list of simple statements p. 176.
    and doStm = function
        | SEQ (a, b)     -> doStm a % doStm b
        | JUMP (e, labs) -> let (stm', e') = doExp e
                            stm' % JUMP (e', labs)
        | CJUMP (p, a, b, t, f) ->
                            match doExps [a; b] with
                            | stm', [a'; b'] -> stm' % CJUMP (p, a', b', t, f)
                            | _              -> failwithf "ERROR: Illegal argument in CJUMP."
        | MOVE (TEMP t, b) ->
                            let (stm', rv) = doRVal b
                            stm' % MOVE (TEMP t, rv)
        | MOVE (MEM e, b) ->
                            let (stm1', e') = doExp e
                            let (stm2', rv) = doRVal b
                            if (commutes stm2' e') then
                                stm1' % stm2' % MOVE (MEM e', rv)
                            else
                                let t = TEMP (Temp.newTemp()) in
                                stm1' % MOVE (t, e') % stm2' % MOVE (MEM t, rv)
        | MOVE (ESEQ _ as e, b) ->
                            let (stm1', e') = doExp e
                            stm1' % doStm (MOVE (e', b))
        | EXP e ->
                            let (stm', _) = doExp e
                            stm'
        | LABEL _ as label -> label
        | _                -> failwithf "ERROR: Unexpected expression."

    // Boil out the side-effects into a Stm list, and render the left-over, pure expression.
    and doExp = function
        | BINOP (p, a, b) -> match doExps [a; b] with
                             | stm', [a'; b'] -> (stm', BINOP (p, a', b'))
                             | _              -> failwithf "ERROR: Illegal operand in BINOP."
        | MEM a ->
                             let (stm', a') = doExp a
                             (stm', MEM a')
        | ESEQ (s, e) ->
                             let stm1' = doStm s
                             let (stm2', e') = doExp e
                             (stm1' % stm2', e')

        // Moving calls to top level p. 178
        | CALL (f, args)  -> match doExps (f :: args) with
                             | stm', f' :: args' -> let t = TEMP (Temp.newTemp())
                                                    (stm' % (MOVE (t, CALL (f', args'))), t)
                             | _                 -> failwithf "ERROR: Illegal parameter in CALL."

        | TEMP _ | CONST _ | NAME _ as other -> (nop, other)

    // Handle expression that is the source of a MOVE statement. Boil out all of its
    // side effects into a Stm list, and render the result as a suitable return value.
    and doRVal = function
        | ESEQ (s, e)    -> let stm1' = doStm s
                            let (stm2', rv) = doRVal e
                            (stm1' % stm2', rv)

        | CALL (f, args) -> match doExps (f :: args) with
                            | stm', f' :: args' -> (stm', CALL (f', args'))
                            | _                 -> failwithf "ERROR: Illegal parameter in RHS CALL."

        | exp            -> doExp exp

    // SEQ(s1, SEQ(s2, SEQ(sn-1, sn)...)) to [s1; s2; ..., sn-1, sn]
    let rec linear stm l =
        match stm with
        | SEQ (a, b) -> linear a (linear b l)
        | s          -> s :: l

    linear (doStm stm0) []

// Same as the Appel's code

type block = Stm list

// From a list of cleaned trees, produce a list of basic blocks satisfying
// the following properties:
//     1. and 2. as above;
//     3. Every block begins with a LABEL;
//     4. A LABEL appears only at the beginning of a block;
//     5. Any JUMP or CJUMP is the last stm in a block;
//     6. Every block ends with a JUMP or CJUMP;
//
// Also produce the "label" to which control will be passed upon exit.
let basicBlocks stms =
    let doneLab = Temp.newLabel()

    let rec blocks stms blist =
        match stms with
        | LABEL _ as h :: l -> let rec next stms block =
                                   match stms with
                                   | JUMP _ as h :: l  -> endBlock l (h :: block)
                                   | CJUMP _ as h :: l -> endBlock l (h :: block)
                                   | LABEL lab :: _    -> next ((JUMP (NAME lab, [lab])) :: stms) block
                                   | h :: l            -> next l (h :: block)
                                   | []                -> next [JUMP (NAME doneLab, [doneLab])] block

                               and endBlock stms block =
                                    blocks stms (List.rev block :: blist)

                               next l [h]

        | [] -> List.rev blist
        | _  -> blocks (LABEL (Temp.newLabel()) :: stms) blist

    (blocks stms [], doneLab)

let enterBlock block table =
    match block with
    | LABEL s :: _ -> Store.enter(table, s, block)
    | _            -> table

// Convert list to ([1...n-1], n)
let rec splitLast = function
    | [x]    -> ([], x)
    | h :: t -> let (t', last) = splitLast t
                (h::t', last)
    | []     -> failwithf "ERROR: Can't split emtpy list."

// Every CJUMP is followed by false lablel p. 181
let rec trace table block rest =
    match block with
    | LABEL lab :: _ as b ->
                let table = Store.enter(table, lab, [])

                match splitLast b with
                | most, JUMP (NAME lab, _)    ->
                            match Store.lookup(table, lab) with
                            | Some (_ :: _ as b') -> most @ trace table b' rest
                            | _                   -> b @ getNext table rest

                | most, CJUMP (op, x, y, t, f) ->
                            match Store.lookup(table, t), Store.lookup(table, f) with
                            | _, Some (_ :: _ as b') -> b @ trace table b' rest

                            | Some (_ :: _ as b'), _ -> most @ [CJUMP (notRelop op, x, y, f, t)] @ trace table b' rest

                            | _                      -> let f' = Temp.newLabel()
                                                        most @ [CJUMP (op, x, y, t, f');
                                                                LABEL f';
                                                                JUMP (NAME f, [f])] @ getNext table rest
                | _, JUMP _                    -> b @ getNext table rest
                | _                            -> failwithf "ERROR: Doesn't contain JUMP"

    | _ -> failwithf "ERROR: Block without LABEL."

and getNext table = function
  | (LABEL lab :: _ as b) :: rest ->
                match Store.lookup(table, lab) with
                | Some (_ :: _) -> trace table b rest
                | _             -> getNext table rest

  | [] -> []
  | _  -> failwithf "ERROR: Doesn't start with LABEL."

// From a list of basic blocks satisfying properties 1-6, along with an "exit" label,
// produce a list of stms such that:
//     1. and 2. as above;
//     7. Every CJUMP (_, t, f) is immediately followed by LABEL f.
//
// The blocks are reordered to satisfy property 7; also in this reordering
// as many 'JUMP (NAME lab)' statements as possible are eliminated by falling
// through into 'LABEL lab'.
let traceSchedule (blocks, doneLab) =
    let table = List.foldBack enterBlock blocks Store.empty
    getNext table blocks @ [LABEL doneLab]