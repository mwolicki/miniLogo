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

type NumberExpr =
| Num of uint16
| Add of NumberExpr * NumberExpr
| Sub of NumberExpr * NumberExpr


type Color = Green | Black | Blue | Yellow | Gray | Red | White
with 
  member x.Name = 
    match x with
    | Green -> "green" | Black -> "black" |  Blue -> "blue" | Yellow -> "yellow"  | Gray -> "gray"
    | Red -> "red" | White -> "white"

type Expr = 
| PenUp
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

type PenState = Up | Down | Erase
type Env = { X : float; Y : float; Pen : PenState; IsVisible : bool; Procedures : Map<string, ProcedureDef>; Angle : int16; BackgroudColor : Color; PenColor : Color; PenSize : uint16 }
let empty = { X = 0.; Y = 0.; Pen = Down; IsVisible = true; Procedures = Map.empty; Angle = 180s; BackgroudColor = White; PenColor = Black; PenSize = 2us }

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

let inline drawContext (ctx:Browser.Types.CanvasRenderingContext2D) env  f =
    ctx.beginPath ()
    ctx.lineWidth <- float env.PenSize
    ctx.fillStyle <- U3.Case1 env.BackgroudColor.Name
    ctx.strokeStyle <- U3.Case1 env.PenColor.Name
    f ctx
    ctx.stroke ()
    ctx.closePath ()

type Async<'a> =
  static member Singleton x = async { return x }


let fill x y width (arr:uint8 []) r g b =
  let pos x y = (y * width + x) * 4
  let r', g', b', a' = arr.[pos x y], arr.[pos x y + 1], arr.[pos x y + 2], arr.[pos x y + 3]
  printfn "%A" (r', g', b', a')
  
  let pixels = ResizeArray()
  let pixelsVisited = System.Collections.Generic.HashSet()

  pixels.Add((x,y)) |> ignore
  pixelsVisited.Add((x,y)) |> ignore
  let mutable i = 0
  while i < pixels.Count do
    let x, y = pixels.[i] 
    let pos = pos x y
    i <- i + 1
    if pos > arr.Length then ()
    elif arr.[pos] = r' && arr.[pos + 1] = g' && arr.[pos + 2] = b' && arr.[pos + 3] = a' then
      arr.[pos] <- r
      arr.[pos + 1] <- g
      arr.[pos + 2] <- b
      arr.[pos + 3] <- 255uy
      for x' in -1 .. 1 do
        for y' in -1 .. 1 do
          if pixelsVisited.Add((x + x', y + y')) then
            pixels.Add((x + x', y + y'))
  pixels.Clear ()
      


let exec (myCanvas : Browser.Types.HTMLCanvasElement) (expr:Expr list) =

  let ctx = myCanvasBuffer.getContext_2d()

  
  let rec exec (env:Env) xs = async {
    match xs with 
    | [] -> 
      ctx.stroke()
      ctx.closePath()
      return env
    | x::xs -> 
        let! env= 
          match x with
          | BackgroudColor color -> { env with BackgroudColor = color } |> Async.Singleton
          | PenColor color -> { env with PenColor = color }  |> Async.Singleton
          | PenSize (Num size) -> { env with PenSize = size }  |> Async.Singleton
          | Sleep (Num sleep) -> 
            if env.IsVisible then drawTurtle env

            let mainCtx = myCanvas.getContext_2d()

            mainCtx.clearRect(0., 0., myCanvas.width, myCanvas.height)
            
            mainCtx.drawImage(U3.Case2 myCanvasBuffer, 0., 0., w * ratio, h  * ratio,  0., 0., w, h)
            async {
              do! Async.Sleep (int sleep)
              return env }
          | ColorFill -> 
            ctx.stroke()
            let id = ctx.getImageData(0., 0.,  myCanvas.width,  myCanvas.height)
            fill (int env.X) (int env.Y) (int id.width) id.data 255uy 0uy 0uy
            ctx.putImageData (id, 0., 0.)
            env  |> Async.Singleton//TODO: impl
          | ProcedureCall (name, args) -> env  |> Async.Singleton//TODO: impl
          | Circle (Num radius) ->
            drawContext ctx env (fun ctx ->
              ctx.arc (float env.X, float env.Y, float radius, 0., 2. * Math.PI)
              ctx.moveTo(float env.X, float env.Y))
            env |> Async.Singleton
          | Disk (Num radius) ->
            drawContext ctx env (fun ctx ->
              ctx.arc (float env.X, float env.Y, float radius, 0., 2. * Math.PI)
              ctx.fillStyle <- U3.Case1 env.BackgroudColor.Name
              ctx.fill()
              ctx.moveTo(float env.X, float env.Y))
            env |> Async.Singleton
          | PenUp -> { env with Pen = Up } |> Async.Singleton
          | PenDown -> { env with Pen = Down } |> Async.Singleton
          | PenErase -> { env with Pen = Erase } |> Async.Singleton
          | HideTurtle -> { env with IsVisible = false } |> Async.Singleton
          | ShowTurtle -> { env with IsVisible = true } |> Async.Singleton
          | CleanScreen -> 
            let x = myCanvas.width / 2.
            let y = myCanvas.height / 2.
            ctx.beginPath()
            ctx.moveTo(x, y)
            ctx.clearRect(0., 0., myCanvas.width, myCanvas.height)
            { empty with X = x; Y = y } |> Async.Singleton
          | Procedure p -> { env with Procedures = Map.add p.Name p env.Procedures } |> Async.Singleton
          | Left (Num angle) -> { env with Angle = (env.Angle + int16 angle) % 360s } |> Async.Singleton
          | Right (Num angle) -> { env with Angle = (env.Angle - int16 angle) % 360s } |> Async.Singleton
          | Back (Num steps) -> 
            let steps = float steps
            let angleRadian = radConv * float env.Angle
            let moveY  = steps * (cos angleRadian)
            let moveX  = steps * (sin angleRadian)
            let x = env.X - moveX
            let y = env.Y - moveY

            if env.Pen = Up then ctx.moveTo(x, y)
            else 
              drawContext ctx env (fun ctx ->
                ctx.moveTo(float env.X, float env.Y)
                ctx.lineTo(x, y))

            { env with X = x; Y = y } |> Async.Singleton

          | Forward (Num steps) -> 
            let steps = float steps
            let angleRadian = radConv * float env.Angle
            let moveY  = steps * (cos angleRadian)
            let moveX  = steps * (sin angleRadian)
            let x = env.X + moveX
            let y = env.Y + moveY
            
            if env.Pen = Up then ctx.moveTo(x, y)
            else 
              drawContext ctx env (fun ctx ->
                ctx.moveTo(float env.X, float env.Y)
                ctx.lineTo(x, y))

            { env with X = x; Y = y } |> Async.Singleton
          | Loop (Num n, expr) -> loop n env expr
        return! exec env xs }
  and loop n (env:Env) expr = async {
    if n <= 0us then return env
    else 
      let! env = exec env expr
      return! loop (n - 1us) env expr }
      
  async {
    try
      try
        myButton.disabled <- true
        let! env = exec empty (CleanScreen :: expr)

        if env.IsVisible then drawTurtle env

        let mainCtx = myCanvas.getContext_2d()

        mainCtx.clearRect(0., 0., myCanvas.width, myCanvas.height)
        
        mainCtx.drawImage(U3.Case2 myCanvasBuffer, 0., 0., w * ratio, h  * ratio,  0., 0., w, h)
      finally
        myButton.disabled <- false
    with e ->
      eprintfn "An exception %A occured when processing %A" e expr } |> Async.Start
  

module P =
  open Parser

  let pUint16 = pUint16 ==> Num

  let pColor = 
    pChar '"' =>. pAny [
      pStr "NIEBIESKI" --> Blue
      pStr "ZIELONY" --> Green
      pStr "SZARY" --> Gray
      pStr "CZARNY" --> Black
      pStr "ŻÓŁTY" --> Yellow
      pStr "BIAŁY" --> White
      pStr "CZERWONY" --> Red ]

  let pExpr = 
    let pExpr, pExprSetter = pRef ()
    pAny [
      pStr "NAPRZÓD" .=> pWhitespace =>. pUint16 ==> Forward
      pStr "NP" .=> pWhitespace =>. pUint16 ==> Forward
      pStr "PRAWO" .=> pWhitespace =>. pUint16 ==> Right
      pStr "PW" .=> pWhitespace =>. pUint16 ==> Right
      pStr "LEWO" .=> pWhitespace =>. pUint16 ==> Left
      pStr "LW" .=> pWhitespace =>. pUint16 ==> Left
      pStr "CS" --> CleanScreen
      pStr "POWTÓRZ" .=> pWhitespace1 =>. pUint16 .=> pWhitespace .=>  pChar '[' .=> pWhitespace .=>. pAll (pExpr .=> pWhitespace) .=> pChar ']' ==> Loop 
      pStr "PODNIEŚ" --> PenUp
      pStr "POD" --> PenUp
      pStr "OPUŚĆ" --> PenDown
      pStr "OPU" --> PenDown
      pStr "POKAŻMNIE" --> ShowTurtle
      pStr "PŻ" --> ShowTurtle
      pStr "SCHOWAJMNIE" --> HideTurtle
      pStr "SŻ" --> HideTurtle
      pStr "CZEKAJ" .=> pWhitespace1 =>. pUint16 ==> Sleep
      pStr "KOŁO" .=> pWhitespace1 =>. pUint16 ==> Disk
      pStr "OKRĄG" .=> pWhitespace1 =>. pUint16 ==> Circle
      pStr "USTALGRUBOŚĆ" .=> pWhitespace1 =>. pUint16 ==> PenSize
      pStr "UGP" .=> pWhitespace1 =>. pUint16 ==> PenSize
      pStr "USTALKOLPIS" .=> pWhitespace1 =>. pColor ==> PenColor
      pStr "UKP" .=> pWhitespace1 =>. pColor ==> PenColor
      pStr "USTALKOLMAL" .=> pWhitespace1 =>. pColor ==> BackgroudColor
      pStr "UKM" .=> pWhitespace1 =>. pColor ==> BackgroudColor
      pStr "ZAMALUJ" --> ColorFill
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
