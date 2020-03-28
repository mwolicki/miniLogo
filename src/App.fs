module App

open Fable.Core.JsInterop
open Fable.Import


open Fable.Core

type LZString = 
  abstract compressToBase64 : string -> string
  abstract decompressFromBase64 : string -> string

[<ImportAll("lz-string")>]
let LZMA: LZString = jsNative


let window = Browser.Dom.window

// Get our canvas context 
// As we'll see later, myCanvas is mutable hence the use of the mutable keyword
// the unbox keyword allows to make an unsafe cast. Here we assume that getElementById will return an HTMLCanvasElement 
let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html
let mutable myButton : Browser.Types.HTMLInputElement = unbox window.document.getElementById "go"  // myCanvas is defined in public/index.html
let mutable myResults : Browser.Types.HTMLPreElement = unbox window.document.getElementById "results"  // myCanvas is defined in public/index.html
let myCanvasBuffer : Browser.Types.HTMLCanvasElement = unbox window.document.createElement "canvas"
let myCanvasBufferImage : Browser.Types.HTMLCanvasElement = unbox window.document.createElement "canvas"


// Get the context
let ctx = myCanvas.getContext_2d()

// All these are immutables values
let w = myCanvas.width
let h = myCanvas.height
let ratio = window.devicePixelRatio
let steps = 20
let squareSize = 20

// gridWidth needs a float wo we cast tour int operation to a float using the float keyword
let gridWidth = float (steps * squareSize) 

// resize our canvas to the size of our grid
// the arrow <- indicates we're mutating a value. It's a special operator in F#.
myCanvas.width <- gridWidth
myCanvas.height <- gridWidth
myCanvas.width <- w * ratio
myCanvas.height <- h * ratio
ctx.scale(ratio, ratio)

myCanvas?style?height <- sprintf "%dpx" (int h)
myCanvas?style?width <- sprintf "%dpx" (int w)

myCanvasBuffer.width <- w * ratio
myCanvasBuffer.height <- h * ratio
myCanvasBuffer?style?height <- sprintf "%dpx" (int h)
myCanvasBuffer?style?width <- sprintf "%dpx" (int w)

// print the grid size to our debugger console
printfn "%i" steps

// prepare our canvas operations
[0..steps] // this is a list
  |> Seq.iter( fun x -> // we iter through the list using an anonymous function
      let v = float ((x) * squareSize) 
      ctx.moveTo(v, 0.)
      ctx.lineTo(v, gridWidth)
      ctx.moveTo(0., v)
      ctx.lineTo(gridWidth, v)
    ) 
ctx.strokeStyle <- !^"#ddd" // color

// draw our grid
ctx.stroke() 

// write Fable
ctx.textAlign <- "center"

printfn "done!"



type NumberExpr =
| Num of uint16
| Add of NumberExpr * NumberExpr
| Sub of NumberExpr * NumberExpr

type Expr = 
| PenUp
| PenDown
| PenErase
| HideTurtle
| ShowTurtle
| CleanScreen
| Forward of steps : NumberExpr
| Back of steps : NumberExpr
| Right of angle : NumberExpr
| Left of angle : NumberExpr
| Loop of NumberExpr * Expr list
| Procedure of ProcedureDef
and ProcedureDef = { Name:string; Args:string list; Code:Expr list }

type PenState = Up | Down | Erase
type Env = { X : float; Y : float; Pen : PenState; IsVisible : bool; Procedures : Map<string, ProcedureDef>; Angle : int16 }
let empty = { X = 0.; Y = 0.; Pen = Down; IsVisible = true; Procedures = Map.empty; Angle = 180s }

let rec numExpr = function
  | Add (a, b) -> (numExpr a) + (numExpr b)
  | Sub (a, b) -> (numExpr a) - (numExpr b)
  | Num a -> a

let (|Num|) = numExpr

open System

let radConv = Math.PI / 180.

let drawTurtle env =
    let ctx = myCanvasBuffer.getContext_2d()
    let image:Browser.Types.HTMLImageElement = window?turtleImage
    
    myCanvasBufferImage.width <- 32.
    myCanvasBufferImage.height <- 32.
    
    let imgCtx = myCanvasBufferImage.getContext_2d()
    imgCtx.clearRect(0.,0., 32., 32.)
    imgCtx.save()
    imgCtx.translate(16.,16.)
    imgCtx.rotate (float (180s-env.Angle) * radConv)
    imgCtx.drawImage(U3.Case1 image,-16.,-16.,32., 32.)
    imgCtx.restore()
    ctx.drawImage(U3.Case2 myCanvasBufferImage, env.X - 16.,env.Y - 16., 32., 32.)

let exec (myCanvas : Browser.Types.HTMLCanvasElement) (expr:Expr list) = 

  let ctx = myCanvasBuffer.getContext_2d()

  
  let rec exec env = function 
  | [] -> 
    ctx.strokeStyle <- !^"#000" // color
    ctx.stroke()
    env
  | x::xs -> 
      let env = 
        match x with
        | PenUp -> { env with Pen = Up }
        | PenDown -> { env with Pen = Down }
        | PenErase -> { env with Pen = Erase }
        | HideTurtle -> { env with IsVisible = false }
        | ShowTurtle -> { env with IsVisible = true }
        | CleanScreen -> 
          let x = myCanvas.width / 2.
          let y = myCanvas.height / 2.
          ctx.beginPath()
          ctx.moveTo(x, y)
          
          //ctx.save()
          //ctx.setTransform(1., 0., 0., 1., 0., 0.)
          ctx.clearRect(0., 0., myCanvas.width, myCanvas.height)
          //ctx.restore()
          { empty with X = x; Y = y }
        | Procedure p -> { env with Procedures = Map.add p.Name p env.Procedures }
        | Left (Num angle) -> { env with Angle = (env.Angle + int16 angle) % 360s }
        | Right (Num angle) -> { env with Angle = (env.Angle - int16 angle) % 360s }
        | Back (Num steps) -> 
          let steps = float steps
          let angleRadian = radConv * float env.Angle
          let moveY  = steps * (cos angleRadian)
          let moveX  = steps * (sin angleRadian)
          let x = env.X - moveX
          let y = env.Y - moveY

          if env.Pen = Up then ctx.moveTo(x, y)
          else ctx.lineTo(x, y)

          { env with X = x; Y = y }

        | Forward (Num steps) -> 
          let steps = float steps
          let angleRadian = radConv * float env.Angle
          let moveY  = steps * (cos angleRadian)
          let moveX  = steps * (sin angleRadian)
          let x = env.X + moveX
          let y = env.Y + moveY
          
          if env.Pen = Up then ctx.moveTo(x, y)
          else ctx.lineTo(x, y)

          { env with X = x; Y = y }
        | Loop (Num n, expr) -> loop n env expr
      exec env xs      
  and loop n env expr =
    if n <= 0us then env
    else 
      let env = exec env expr
      loop (n - 1us) env expr
      
  
  let env = exec empty expr

  if env.IsVisible then
    drawTurtle env

  let mainCtx = myCanvas.getContext_2d()

  mainCtx.clearRect(0., 0., myCanvas.width, myCanvas.height)
  
  mainCtx.drawImage(U3.Case2 myCanvasBuffer, 0., 0., w * ratio, h  * ratio,  0., 0., w, h)
  

module P =
  open Parser

  let pUint16 = pUint16 ==> Num


  let pExpr = 
    let pExpr, pExprSetter = pRef ()
    pAny [
      pStr "POD" --> PenUp
      pStr "OPU" --> PenDown
      pStr "ŚĆIER" --> PenErase
      pStr "PŻ" --> ShowTurtle
      pStr "SŻ" --> HideTurtle
      pStr "CS" --> CleanScreen
      pStr "NP" .=> pWhitespace =>. pUint16 ==> Forward
      pStr "PW" .=> pWhitespace =>. pUint16 ==> Right
      pStr "LW" .=> pWhitespace =>. pUint16 ==> Left
      pStr "POWTÓRZ" .=> pWhitespace1 =>. pUint16 .=> pWhitespace .=>  pChar '[' .=>. pAll (pExpr .=> pWhitespace) .=> pChar ']' ==> Loop 
      pStr "OTO" .=> pWhitespace1 =>. pRegEx "\w+" .=> pWhitespace1 .=>.  pAll (pChar ':' =>. pRegEx "\w+" .=> pWhitespace) .=>. pAll (pExpr .=> pWhitespace) .=> pStr "JUŻ" 
        ==> fun ((name, args), code) -> {Name = name; Args = args; Code = code } |> Procedure
      ] |> pExprSetter

    pExpr

  let parse = pWhitespace =>. pAll (pExpr .=> pWhitespace) .=> pEod



myButton.addEventListener("click", fun _ -> 
  let sourceCode = window?editor?getValue() |> fun (x:string) -> x.ToUpper ()
  window.location.hash <- LZMA.compressToBase64 sourceCode
  let r = P.parse sourceCode
  printfn "result = %A" r
  r |> Result.map (fst>>exec myCanvas) |> ignore)

let windHash = window.location.hash
if isNull windHash |> not && windHash.Length > 1 then
  let code = windHash.[1..]
  let code = LZMA.decompressFromBase64 code
  printf "%A" code
  window?editor?setValue code
  P.parse code |> Result.map (fst>>exec myCanvas) |> ignore
