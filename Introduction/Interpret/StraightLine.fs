module StraightLine

open Absyn
open FrontEnd

// _____________________________________________________________________________
//                                                            Abstract Language

(* Tip: Match all nonterminals, the rest just skip and continue *)

// 1. Iterate abstract syntax tree
let rec iterStmt (stmts: Stmt) =
    match stmts with
    | CompoundStmt(s1, s2) -> iterStmt s1; iterStmt s2
    | AssignStmt(_, exp)   -> iterExpr exp
    | PrintStmt(expList)   -> List.iter iterExpr expList

and private iterExpr (expr: Exp) =
    match expr with
    | OpExp(exp1, _, exp2) -> iterExpr exp1; iterExpr exp2
    | SeqExp(stmt, exp)    -> iterStmt stmt; iterExpr exp
    | _                    -> ()           // terminal

// 2. Count all statements (Stmt is 1, Exp do not change and terminals are 0)
// Tip: Recursion "do the job" when stack unwinds
let stmtCounter (stmt: Stmt) =

    let rec countStmt (stmts: Stmt) (num: int) :int =
        match stmts with
        | CompoundStmt(s1, s2) -> (countStmt s1 num) + (countStmt s2 num)
        | AssignStmt(_, exp)   -> countExpr exp num+1
        | PrintStmt(expList)   -> List.map (fun exp -> countExpr exp num) expList
                                      |> List.fold (+) 1

    and countExpr (expr: Exp) (numExp: int) =
        match expr with
        | OpExp(exp1, _, exp2) -> (countExpr exp1 numExp) + (countExpr exp2 numExp)
        | SeqExp(stmt, exp)    -> (countStmt stmt numExp) + (countExpr exp numExp)
        | _                    -> 0

    countStmt stmt 0

// 3. Tells the maximum number of arguments of any 'print' statement
//    within any sub-expression
let rec maxArgs (stmt: Stmt) :int =
    match stmt with
    | CompoundStmt(s1, s2) -> max (maxArgs s1) (maxArgs s2)
    | AssignStmt(_, exp)   -> maxArgsExp exp
    | PrintStmt(expList)   -> let cur = List.length expList
                              let rest = List.map maxArgsExp expList |> List.max
                              max cur rest

and private maxArgsExp (expr: Exp) =
    match expr with
    | OpExp(exp1, _, exp2) -> max (maxArgsExp exp1) (maxArgsExp exp2)
    | SeqExp(stmt, exp)    -> max (maxArgs stmt) (maxArgsExp exp)
    | _                    -> 0

// _____________________________________________________________________________
//                                                                  Interpreter

type Env<'v> = (string * 'v) list

let rec lookup env var =
    match env with
    | []             -> failwithf "Variable '%s' is not defined" var
    | (k, v) :: rest -> if k = var then v else lookup rest var

let update env var newValue =
    List.map (fun (k, i) -> if k = var then (k, newValue) else (k, i)) env

// Tip: Expression can't change environment

let interp (prog: Stmt) =

    let rec interpStmt (stmts: Stmt) (env: Env<int>) =
        match stmts with
        | CompoundStmt(s1, s2) -> interpStmt s1 env |> interpStmt s2
        | AssignStmt(id, exp)  -> (id, fst (interpExpr exp env)) :: env
        | PrintStmt(expList)   -> List.iter (fun e -> printf "%i " (fst (interpExpr e env))) expList
                                  env

    and interpExpr (expr: Exp) (env: Env<int>) =
        match expr with
        | IdExp(id)                -> (lookup env id, env)
        | NumExp(num)              -> (num, env)

        | OpExp(exp1, binOp, exp2) -> let e1 = interpExpr exp1 env
                                      let e2 = interpExpr exp2 env
                                      match binOp with
                                      | Plus  -> (fst e1 + fst e2, env)
                                      | Minus -> (fst e1 - fst e2, env)
                                      | Times -> (fst e1 * fst e2, env)
                                      | Div   -> (fst e1 / fst e2, env)

        | SeqExp(stmt, exp)        -> interpStmt stmt env |> interpExpr exp

    interpStmt prog []

// _____________________________________________________________________________
//                                                                          Run

let run prog =
    LexParse.tryParse prog
        |> interp
