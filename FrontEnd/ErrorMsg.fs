module ErrorMsg

let anyErrors =
    ref false

let reset =
    anyErrors := false

let error (line, column) (msg: string) =
    anyErrors := true
    printf "[Line:%i, Column:%i] %s\n" line column msg
