module TigerSemanticTests

open System
open FsUnit.Xunit
open Xunit

open Env
open Types

[<Fact>]
let ``Get actual type`` () =
    let venv = Store.empty<VarEntry>
    let venv' = Store.enter (venv, ("a", 0), {ty=INT; access=()})
    printf "Environment: %A" venv'
