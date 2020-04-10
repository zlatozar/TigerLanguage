namespace Tiger

module BackEnd =

    let emitToConsole (frag: Frame.Frag) =

        match frag with
        | Frame.PROC {body=body; frame=frame} ->

                let instrs = List.collect
                                 (Codegen.codegen frame)
                                     (body
                                         |> Canon.linearize
                                         |> Canon.basicBlocks
                                         |> Canon.traceSchedule)

                let (instrs', allocation) = RegAlloc.alloc (Frame.procEntryExit2 frame instrs) frame
                let (prologue, instrs'', epilogue) = Frame.procEntryExit3 frame instrs'

                let stringOfTemp t = Temp.Table.lookup allocation t

                printfn "%s" prologue
                List.iter (fun i -> printfn "%s" (Assem.format stringOfTemp i)) instrs''
                printfn "%s" epilogue

        | Frame.STRING (label, s)             -> printf "%s" (Frame.string (label, s))

    // TODO
    let emitToFile out (frag: Frame.Frag) =
        ()
