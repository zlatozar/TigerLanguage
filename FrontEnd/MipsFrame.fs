module Frame

type Access = 
    | InFrame of int 
    | InReg of Temp.Temp

type Frame = { name: Temp.Label; formals: Access list;
               locals: int ref; instrs: Tree.Stm list }

type Register = string

type Frag = 
    | PROC of ProcRec
    | STRING of Temp.Label * string

and ProcRec = 
    { body: Tree.Stm; frame: Frame}    

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

// Functions (dummy)

let newFrame (frameRec: FrameRec) = { name=(Store.symbol "dummy"); formals=[]; locals=ref 0; instrs=[] }
let name (f: Frame) = f.name

let formals (f: Frame) = f.formals

let allocLocal (f: Frame) (escape: bool) = InFrame(0)