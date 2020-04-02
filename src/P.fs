module P

open Parser
open Expr

let pProcedureNameCall : string Parse = pRegEx "\w+" >> Result.bind(function ("JUŻ", _) -> Error "JUŻ is special keyword" | x -> Ok x)

let pColor = 
    pChar '"' =>. pAny [
      pStr "NIEBIESKI" --> Blue
      pStr "ZIELONY" --> Green
      pStr "SZARY" --> Gray
      pStr "CZARNY" --> Black
      pStr "ŻÓŁTY" --> Yellow
      pStr "BIAŁY" --> White
      pStr "CZERWONY" --> Red ]

let pNum : NumberExpr Parse = 

    let pExpr, pExprSetter = pRef ()
    let pUint16 = pUint16 ==> Num
    let pBracket = pChar '(' =>. pWhitespace =>. pExpr .=> pWhitespace .=> pChar ')'
    let pVar = pChar '"' =>. pRegEx "\w+" ==> Variable


    pAny [
      pUint16 .=> pWhitespace .=> pChar '+' .=> pWhitespace .=>. pExpr ==> Add
      pUint16 .=> pWhitespace .=> pChar '-' .=> pWhitespace .=>. pExpr ==> Sub
      pVar .=> pWhitespace .=> pChar '+' .=> pWhitespace .=>. pExpr ==> Add
      pVar .=> pWhitespace .=> pChar '-' .=> pWhitespace .=>. pExpr ==> Sub
      pBracket .=> pWhitespace .=> pChar '+' .=> pWhitespace .=>. pExpr ==> Add
      pBracket .=> pWhitespace .=> pChar '-' .=> pWhitespace .=>. pExpr ==> Sub
      pBracket
      pUint16
      pVar
      ] |> pExprSetter

    pExpr

let pExpr = 
    let pExpr, pExprSetter = pRef ()
    pAny [
      pStr "NAPRZÓD" .=> pWhitespace =>. pNum ==> Forward
      pStr "NP" .=> pWhitespace =>. pNum ==> Forward
      pStr "PRAWO" .=> pWhitespace =>. pNum ==> Right
      pStr "PW" .=> pWhitespace =>. pNum ==> Right
      pStr "LEWO" .=> pWhitespace =>. pNum ==> Left
      pStr "LW" .=> pWhitespace =>. pNum ==> Left
      pStr "POWTÓRZ" .=> pWhitespace1 =>. pNum .=> pWhitespace .=>  pChar '[' .=> pWhitespace .=>. pAll (pExpr .=> pWhitespace) .=> pChar ']' ==> Loop 
      pStr "PODNIEŚ" --> PenUp
      pStr "POD" --> PenUp
      pStr "OPUŚĆ" --> PenDown
      pStr "OPU" --> PenDown
      pStr "POKAŻMNIE" --> ShowTurtle
      pStr "PŻ" --> ShowTurtle
      pStr "SCHOWAJMNIE" --> HideTurtle
      pStr "SŻ" --> HideTurtle
      pStr "CZEKAJ" .=> pWhitespace1 =>. pNum ==> Sleep
      pStr "CZYŚĆ" --> CleanScreen
      pStr "CZ" --> CleanScreen
      pStr "KOŁO" .=> pWhitespace1 =>. pNum ==> Disk
      pStr "OKRĄG" .=> pWhitespace1 =>. pNum ==> Circle
      pStr "USTALGRUBOŚĆ" .=> pWhitespace1 =>. pNum ==> PenSize
      pStr "UGP" .=> pWhitespace1 =>. pNum ==> PenSize
      pStr "USTALKOLPIS" .=> pWhitespace1 =>. pColor ==> PenColor
      pStr "UKP" .=> pWhitespace1 =>. pColor ==> PenColor
      pStr "USTALKOLMAL" .=> pWhitespace1 =>. pColor ==> BackgroudColor
      pStr "UKM" .=> pWhitespace1 =>. pColor ==> BackgroudColor
      pStr "ZAMALUJ" --> ColorFill
      
      pStr "OTO" .=> pWhitespace1 =>. pRegEx "\w+" .=> pWhitespace1 .=>.  pAll (pChar ':' =>. pRegEx "\w+" .=> pWhitespace) .=>. pAll (pExpr .=> pWhitespace) .=> pStr "JUŻ" 
        ==> fun ((name, args), code) -> {Name = name; Args = args; Code = code } |> Procedure
      //needs to be the last one
      pProcedureNameCall .=> pWhitespace1 .=>. pAll (pNum .=> pWhitespace) ==> ProcedureCall
      pRegEx ";.*\n" ==> Comment
      ] |> pExprSetter

    pExpr

let parse (txt:string) = (pWhitespace =>. pAll (pExpr .=> pWhitespace) .=> pEod) (txt.ToUpper () + "\n")

