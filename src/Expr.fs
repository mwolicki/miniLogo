module Expr

type NumberExpr =
| Num of uint16
| Add of NumberExpr * NumberExpr
| Sub of NumberExpr * NumberExpr
| Variable of variable:string


type Color = Green | Black | Blue | Yellow | Gray | Red | White
with 
  member x.Rgb = 
    match x with
    | Green -> 0uy,255uy,0uy | Black -> 0uy,0uy,0uy |  Blue -> 0uy,0uy,255uy | Yellow -> 255uy,255uy,0uy  | Gray -> 220uy,220uy,220uy
    | Red -> 255uy,0uy,0uy | White -> 255uy,255uy,255uy
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
