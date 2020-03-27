module Parser

type 'a Parse = string -> Result<'a * string, string>

let pChar p (txt:string) =
        if txt.[0] = p then Ok (p, txt.[1..])
        else sprintf "Unable to parse char '%c'" p |> Error

let pStr str (txt:string) =
        if txt.StartsWith str then Ok (str, txt.[str.Length..])
        else sprintf "Unable to parse string '%s'" str |> Error

let pRegEx regExp  = 
    let regex = System.Text.RegularExpressions.Regex("^" + regExp)

    fun txt -> 
        match regex.Match txt with
        | x when x.Success -> 
            let m = x.Value
            Ok (m, txt.[m.Length..])
        | _ -> sprintf "Unable to parse '%s'" regExp |> Error


let (==>) (parser: 'a Parse) (f: 'a -> 'b) : 'b Parse = 
    parser >> Result.map (fun (state, nextToken) -> f state, nextToken)

let (-->) (parser: 'a Parse) s : 'b Parse = 
    parser >> Result.map (fun (state, nextToken) -> s, nextToken)

let (=>.) (a : 'a Parse) (b : 'b Parse) : Parse<'b> =
    a >> Result.bind (fun (_, nextToken) -> b nextToken) 

let (.=>) (a : 'a Parse) (b : 'b Parse) : Parse<'a> =
    a >> Result.bind (fun (state, nextToken) -> 
        b nextToken 
        |> Result.map(fun (_, nextToken) -> state, nextToken)) 

let (.=>.) (a : 'a Parse) (b : 'b Parse) : Parse<'a*'b> =
    a >> Result.bind (fun (stateA, nextToken) -> 
        b nextToken 
        |> Result.map(fun (stateB, nextToken) -> (stateA, stateB), nextToken))

let pAny (parsers : 'a Parse list) : 'a Parse =
    fun txt ->
        let rec loop = function 
        | [] -> Error "None of the parsers has succeeded."
        | p::ps ->
            match p txt with
            | Ok _ as ok -> ok
            | Error _ -> loop ps
        loop parsers

let pAll (parser : 'a Parse) : ('a list) Parse =
    let rec loop acc txt = 
        match parser txt with
        | Error _ -> Ok (List.rev acc, txt)
        | Ok (state, nextToken) ->
            let acc = state::acc
            loop acc nextToken
    loop []

let pUint16 : Parse<uint16> = 
    pRegEx "[1-9][0-9]{0,3}" 
    >> Result.bind(fun (state, nextToken) -> 
        match System.UInt16.TryParse state with
        | true, value -> Ok (value, nextToken)
        | false, _ -> sprintf "Number %s is too large!" state |> Error)

let pWhitespace = pRegEx "[\t\n\r ]*"
let pWhitespace1 = pRegEx "[\t\n\r ]+"
let pEod : Parse<unit> = fun txt -> 
    if txt = "" then Ok ((), txt) 
    else sprintf "Text is not empty: '%s'" txt |> Error


let pRef () =
    let mutable p : _ Parse = Unchecked.defaultof<_>

    (fun txt -> p txt), (fun x -> p <- x)