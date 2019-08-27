module RegAlloc

type Allocation = Temp.Table<Frame.Register>

type Teplace = {
    replacement: Temp.Table<Temp.Temp>;
    instrs: Assem.Instr list;
    temps: Temp.Temp list
}