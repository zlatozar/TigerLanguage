module Assem

open Temp
open System.Text.RegularExpressions

type Reg = string

type OperInstr = {
    assem: string;   // Assembly-lang instruction
    dst: Temp list;  // A list of result registers (may be empty)
    src: Temp list;  // A list of operands registers (may be empty)
    jump: Label list option  // 'None' if fall to the next following instruction
}

type LabelInstr = {
    assem: string;
    lab: Label
}

type MoveInstr = {
    assem: string;
    dst: Temp;
    src: Temp
}

// All instructions types that we will iterate
type Instr =
    | OPER of OperInstr
    | LABEL of LabelInstr
    | MOVE of MoveInstr

// Confention: 'd' - destination, 's' - source and 'j' (jump) refer to lable

let placeholderRegex = "'([dsj])([0-9]+)"
let padding = "    "

let format formatTemp instr =

    let fillPlaceholders assem src dst jump =
        let matchEvaluator =
            MatchEvaluator(fun placeholder -> let kind = placeholder.Groups.[1].Value
                                              let idx = (int) placeholder.Groups.[2].Value

                                              match kind with
                                              | "s" -> formatTemp (List.item idx src)
                                              | "d" -> formatTemp (List.item idx dst)
                                              | "j" -> Temp.stringOfLabel (List.item idx jump)
                                              | _   -> failwithf "ERROR: Bad Assem format.")

        Regex.Replace(assem, placeholderRegex, matchEvaluator)

    match instr with
    | OPER {assem=assem; src=src; dst=dst; jump=jump}
                        -> sprintf "%s%s%s" padding padding (fillPlaceholders assem src dst (Option.defaultValue [] jump))
    | LABEL {assem=assem; lab=_}
                        -> assem
    | MOVE {assem=assem; src=src; dst=dst}
                        -> sprintf "%s%s%s" padding padding (fillPlaceholders assem [src] [dst] [])
