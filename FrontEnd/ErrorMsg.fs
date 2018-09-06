module ErrorMsg

let anyErrors = ref false
let fileName = ref ""
let lineNum = ref 1
let linePos = ref [1]

let reset = anyErrors := false
            fileName := ""
            lineNum := 1
            linePos := [1]

let errorOrig pos (msg: string) =
  let rec look (lp, ln) =
      match lp with
      | a::rest when a < pos  -> printf "[Line:%i, Column:%i]" ln (pos - a)
      | a::rest               -> look (rest, ln - 1)
      | _                     -> printf "0.0"

  anyErrors := true
  printf "%s" !fileName
  look (!linePos, !lineNum)
  printf ": %s\n" msg

let error (line, column) (msg: string) =
    anyErrors := true
    printf "[Line:%i, Column:%i] %s\n" line column msg
