namespace Tiger

module Semantic =

    open TigerSemantic

    // ______________________________________________________________________________
    //

    let fromString (str: string) =
        Tiger.Parser.fromString str

    let fromFile (filename: string) =
        Tiger.Parser.fromFile filename
