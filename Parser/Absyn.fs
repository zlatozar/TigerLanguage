module Absyn

(* Definition of Tiger Abstract Syntax Types *)

open Store

// _____________________________________________________________________________
//                                                         Abstract Syntax Tree

// Needed to report semantic errors in next phase - Semantic Analysis
type Pos = int * int

// Tip: Note that we track the positions of IDs and types

type TVar =                             (* Use examples during synctic analysis *)
    | SimpleVar of Symbol * Pos         (*   x              *)
    | FieldVar of TVar * Symbol * Pos   (*   x.all          *)
    | SubscriptVar of TVar * TExp * Pos (*   row[5]         *)

and TExp =
    | IntExp of int                     (*   1, 2, 3 ...    *)
    | StringExp of string * Pos         (*   "\n Enter: "   *)
    | NilExp                            (*   nil            *)
    | BreakExp of Pos                   (*   break          *)
    | VarExp of TVar                    (*   container for TVars           *)

    | AssignExp of AssignRec            (*   a := 5         *)
    | OpExp of OpRec                    (*   a + 4          *)
    | CallExp of CallRec                (*   double(42)     *)
    | RecordExp of RecordRec            (*   person {name="aname", id=0}   *)
    | ArrayExp of ArrayRec              (*   col[i] := 42   *)
    | SeqExp of (TExp * Pos) list       (*   (a := 5; a+1) or ()           *)
    | IfExp of IfRec                    (*   if a=nil then b else d        *)
    | WhileExp of WhileRec
    | ForExp of ForRec
    | LetExp of LetRec                  (*   let var a:=1 in a*2 end       *)

(*
   'FunctionDec' is a list because mutual recursive functions form a tree in a ASTree.
   As you know trees could be described with nested lists(CONSing).
   The same is for recursive type declarations. See p. 97-98.
*)

and TDec =
    | TypeDec of TypeDecRec list        (*   type tree = {key: int, children: treelist} *)
    | VarDec of VarDecRec               (*   var a := 5; Tip: for id:=5 to.. id is var  *)
    | FunctionDec of FunDecRec list     (*   function double(a: int) = a*2              *)

and TType =
    | NameTy of Symbol * Pos            (*   int                          *)
    | RecordTy of FieldRec list         (*   {name: string, id: int}      *)
    | ArrayTy of Symbol * Pos           (*   array of int                 *)

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
                  exp: TExp;
                  pos: Pos }

and OpRec = { left: TExp;
              oper: TOper;
              right: TExp;
              pos: Pos }

and CallRec = { func: Symbol;
                args: TExp list;
                pos: Pos }

and RecordRec = { typ: Symbol;
                  fields: (Symbol * TExp * Pos) list;
                  pos: Pos }

and ArrayRec = { typ: Symbol;
                 size: TExp;
                 init: TExp;
                 pos: Pos }

and IfRec = { test: TExp;
              then': TExp;
              else': TExp option;
              pos: Pos }

and WhileRec = { test: TExp;
                 body: TExp;
                 pos: Pos }

and ForRec = { var: Symbol;
               escape: bool ref;
               lo: TExp;    // initial value of 'var'
               hi: TExp;
               body: TExp;
               pos: Pos }

and LetRec = { decs: TDec list;
               body: TExp;  // It is an 'Exp' not list of expressions p. 519
               pos: Pos }

and TypeDecRec = { name: Symbol;
                   ty: TType;
                   pos: Pos }

and VarDecRec = { name: Symbol;
                  escape: bool ref;
                  typ: (Symbol * Pos) option;
                  init: TExp;
                  pos: Pos }

and FunDecRec = { name: Symbol;
                  param: FieldRec list;
                  result: (Symbol * Pos) option;
                  body: TExp;
                  pos: Pos }

and FieldRec = { name: Symbol;
                 escape: bool ref;
                 typ: Symbol;
                 pos: Pos }
