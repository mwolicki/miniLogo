module Expr

type NumberExpr =
| Num of uint16
| Add of NumberExpr * NumberExpr
| Sub of NumberExpr * NumberExpr
| Variable of variable:string


type Color = Green | Black | Blue | Yellow | Gray | Red | White | Brown | Violet | Orange
with 
  member x.Rgb = 
    match x with
    | Green -> 161uy,222uy,147uy | Black -> 0uy,0uy,0uy |  Blue -> 112uy,161uy,215uy | Yellow -> 247uy,244uy,139uy  | Gray -> 207uy,207uy,196uy
    | Red -> 244uy,124uy,124uy | White -> 255uy,255uy,255uy | Brown -> 131uy,104uy,83uy | Violet -> 179uy,153uy,212uy | Orange -> 255uy,180uy,71uy
  member x.Name = 
    let (r,g,b) = x.Rgb
    sprintf "rgb(%d,%d,%d)" r g b

type Expr = 
| PenUp
| Comment of string
| PenDown
| PenErase
| HideTurtle
| ShowTurtle
| CleanScreen
| ColorFill
| Sleep of milisec : NumberExpr
| ProcedureCall of name : string * NumberExpr list
| BackgroudColor of name : Color
| PenColor of name : Color
| PenSize of size : NumberExpr
| Disk of radius : NumberExpr
| Circle of radius : NumberExpr
| Forward of steps : NumberExpr
| Back of steps : NumberExpr
| Right of angle : NumberExpr
| Left of angle : NumberExpr
| Loop of NumberExpr * Expr list
| Procedure of ProcedureDef
and ProcedureDef = { Name:string; Args:string list; Code:Expr list }
