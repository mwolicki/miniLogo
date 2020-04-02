module Tests

open Fable.Core
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

let example1 = """POD
LW 275
NP 145
OPU
USTALKOLMAL "ZIELONY
POD
LW 90 NP 150 PW 90
OPU
POWTÓRZ 6 [NP 25 PW 90 NP 25 LW 90]
LW 90 NP 25 PW 180
POWTÓRZ 6 [NP 25 PW 90 NP 25 LW 90]
LW 180
NP 275
POD
PW 150
NP 120-1
USTALKOLMAL "CZERWONY
ZAMALUJ"""

let example2 = """ZAMALUJ
CZEKAJ (1000 - 80) + 60

OTO KWADRAT :size
  UKP "ZIELONY
  POWTÓRZ 4 [NP "size PW 90]
JUŻ

OTO KWADRAT2
  KWADRAT 100
JUŻ

;test komentarz

KWADRAT2"""

describe "tests" <| fun () ->
    it "parse 'ZAMALUJ'" <| fun () -> successParse "ZAMALUJ" |> assertEqual  [ColorFill]
    it "parse 'CZYŚĆ'" <| fun () -> successParse "CZYŚĆ" |> assertEqual  [CleanScreen]
    it "parse example #1" <| fun () -> 
        successParse example1 
        |> assertEqual [PenUp; Left (Num 275us); Forward (Num 145us); PenDown; BackgroudColor Green; 
            PenUp; Left (Num 90us); Forward (Num 150us); Right (Num 90us); PenDown; 
            Loop (Num 6us,[Forward (Num 25us); Right (Num 90us); Forward (Num 25us); Left (Num 90us)]); 
            Left (Num 90us); Forward (Num 25us); Right (Num 180us); 
            Loop (Num 6us,[Forward (Num 25us); Right (Num 90us); Forward (Num 25us); Left (Num 90us)]); 
            Left (Num 180us); Forward (Num 275us); PenUp; Right (Num 150us); Forward (Sub (Num 120us,Num 1us));
            BackgroudColor Red; ColorFill]

    it "parse example #2" <| fun () -> 
        successParse example2
        |> assertEqual [ColorFill; Sleep (Add (Sub (Num 1000us,Num 80us),Num 60us)); 
            Procedure {Name = "KWADRAT"; Args = ["SIZE"]; Code = [PenColor Green; Loop (Num 4us,[Forward (Variable "SIZE"); Right (Num 90us)])]}; 
            Procedure {Name = "KWADRAT2"; Args = []; Code = [ProcedureCall ("KWADRAT",[Num 100us])]};
            Comment ";TEST KOMENTARZ\n"; ProcedureCall ("KWADRAT2",[])]
