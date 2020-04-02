module Tests

open Fable.Core
open Fable.Core.JsInterop
open App
open Expr

open Fable.Core
open Fable.Core.Testing

let [<Global>] describe (name: string) (f: unit -> unit) = jsNative
let [<Global>] it (msg: string) (f: unit -> unit) = jsNative

let assertEqual expected actual: unit =
    Assert.AreEqual(actual, expected)
let successParse txt =
    match P.parse txt with
    | Ok (r, "") -> r
    | x -> failwithf "Unexpected result %A" x

describe "tests" <| fun () ->
    it "parse 'ZAMALUJ'" <| fun () -> successParse "ZAMALUJ" |> assertEqual  [ColorFill]
    it "parse 'CZYŚĆ'" <| fun () -> successParse "CZYŚĆ" |> assertEqual  [CleanScreen]
