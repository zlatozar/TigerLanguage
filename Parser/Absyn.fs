module Absyn

(* Definition of Tiger Abstract Syntax Types *)

open Store

// _____________________________________________________________________________
//                                                         Abstract Syntax Tree

type TVar =
    | SimpleVar of Symbol              (*   x              *)
    | FieldVar of TVar * Symbol        (*   x.all          *)
    | SubscriptVar of TVar * TExp      (*   row[5];VarExp(SubscriptVar(SimpleVar(row).. *)

and TExp =
    | IntExp of int                    (*   1, 2, 3 ...    *)
    | StringExp of string              (*   "\n Enter: "   *)
    | NilExp                           (*   nil or represent absence of expressions     *)
    | BreakExp                         (*   break          *)
    | VarExp of TVar                   (*   double(x); x is VarExp(SimpleVar(x)...      *)

    | AssignExp of AssignRec           (*   a := 5         *)
    | OpExp of OpRec                   (*   a + 4          *)
    | CallExp of CallRec               (*   double(42)     *)
    | RecordExp of RecordRec           (*   person {name="aname", id=0}   *)
    | ArrayExp of ArrayRec             (*   col[i]         *)
    | SeqExp of TExp list              (*   (a := 5; a+1)  *)
    | IfExp of IfRec                   (*   if a=nil then b else d        *)
    | WhileExp of WhileRec
    | ForExp of ForRec
    | LetExp of LetRec                 (*   let var a:=1 in a*2 end       *)

(*
   'FunctionDec' is a list because mutual recursive functions form a tree in a ASTree.
   As you know trees could be described with nested lists(CONSing).
   The same is for recursive type declarations. See p. 97-98.
*)

and TDec =
    | TypeDec of TypeRec list          (*   type tree = {key: int, children: treelist} *)
    | VarDec of VarRec                 (*   var a := 5                                 *)
    | FunctionDec of FunDecRec list    (*   function double(a: int) = a*2              *)

and TType =
    | NameTy of Symbol                 (*   int           *)
    | RecordTy of FieldRec list        (*   {name: string, id: int}      *)
    | ArrayTy of Symbol                (*   array of int  *)

and TOper =
    | PlusOp
    | MinusOp
    | TimesOp
    | DivideOp
    | EqOp
    | NeqOp
    | GtOp
    | GeOp
    | LtOp
    | LeOp

// _____________________________________________________________________________
//                                                                      Records

and AssignRec = { var: TVar;
                  exp: TExp }

and OpRec = { left: TExp;
              oper: TOper;
              right: TExp }

and CallRec = { func: Symbol;
                args: TExp list }

and RecordRec = { typ: Symbol;
                  fields: (Symbol * TExp) list }

and ArrayRec = { typ: Symbol;
                 size: TExp;
                 init: TExp }

and IfRec = { test: TExp;
              then': TExp;
              else': TExp option }

and WhileRec = { test: TExp;
                 body: TExp }

and ForRec = { var: Symbol;
               escape: bool ref;
               lo: TExp;
               hi: TExp;
               body: TExp }

and LetRec = { decs: TDec list;
               body: TExp }      // It is an 'Exp' not list of expressions p. 519

and TypeRec = { name: Symbol;
                ty: TType }

and VarRec = { name: Symbol;
               escape: bool ref;
               typ: Symbol option;
               init: TExp }

and FunDecRec = { name: Symbol;
                  param: FieldRec list;
                  result: Symbol option;
                  body: TExp }

and FieldRec = { name: Symbol;
                 escape: bool ref;
                 typ: Symbol }
